#!/usr/bin/env python3
"""Build, run, and animate examples/c/sphere.

Finds sdfast from a CMake build directory (single-config build/sdfast or
build/sdfast.exe, or multi-config build/Release/sdfast.exe and the other
config names), generates the C model, compiles sphere, and feeds one canned
case on stdin. The sdfast path is made absolute before the run, because
generation uses a temporary working directory. A Windows build of sdfast
(MinGW included) writes sphere_d.c and sphere_s.c; a Unix build writes
sphere_dyn.c and sphere_sar.c. Samples come from sphere_anim.dat: center
position and the body-to-ground direction cosine. Angular velocity is not
integrated.

    python3 examples/c/animate_sphere.py --case xspin --build-dir build
    python3 examples/c/animate_sphere.py --no-render --case xspin --sdfast ./sdfast
    python3 examples/c/animate_sphere.py --self-test

Set SDFAST or pass --sdfast to skip the search. Set CC to choose the
compiler. A real GNU gcc is preferred, including versioned gcc-N from
Homebrew, because macOS /usr/bin/gcc is Apple clang. cl is used only if
it is the only compiler on PATH. An MP4 is written when ffmpeg is on PATH; otherwise
a GIF is written with Pillow. --no-render only checks sphere_anim.dat and
does not import numpy. Python is not required to build SD/FAST.
"""

import argparse
import math
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
CONFIGS = ("Release", "Debug", "RelWithDebInfo", "MinSizeRel")
CASES = {
    "xspin": (1, 1, 1, 0, 0, 0, 0, 0, 0, math.pi, 0, 0),
    "zspin": (1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, math.pi),
    "yspin": (1, 1, 1, 0, 0, 0, 0, 0, 0, 0, math.pi, 0),
    "equator": (0.9, 0.8, 0.8, 1, 0, 90, 2, 0, 0, 0, 0, -2),
    "deg45": (0.9, 0.8, 0.8, 1, 0, 45, 0, 0, 0, 0, 0, 0),
}


def _exe_names(stem):
    return (stem, stem + ".exe")


def search_build_tree(root):
    """Single-config binary in the build root wins over a config subdirectory."""
    root = Path(root)
    for name in _exe_names("sdfast"):
        candidate = root / name
        if candidate.is_file():
            return candidate
    for config in CONFIGS:
        for name in _exe_names("sdfast"):
            candidate = root / config / name
            if candidate.is_file():
                return candidate
    return None


def find_sdfast(build_dir, explicit):
    # Always absolute. The generator is run with cwd set to a temp
    # directory, and a relative build/sdfast is then looked up there.
    if explicit:
        path = Path(explicit)
        if not path.is_file():
            raise SystemExit("sdfast not found: %s" % path)
        return path.resolve()
    env = os.environ.get("SDFAST")
    if env and Path(env).is_file():
        return Path(env).resolve()
    roots = []
    if build_dir:
        roots.append(Path(build_dir))
    else:
        for base in (Path.cwd(), ROOT):
            roots.append(base / "build")
    for root in roots:
        found = search_build_tree(root)
        if found:
            return found.resolve()
    which = shutil.which("sdfast")
    if which:
        return Path(which).resolve()
    raise SystemExit(
        "sdfast not found. Pass --sdfast, set SDFAST, or --build-dir "
        "(build/sdfast, build/sdfast.exe, or build/Release/sdfast.exe)."
    )


def _compiler_is_clang(cc):
    try:
        out = subprocess.run(
            [cc, "--version"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
        )
    except OSError:
        return False
    return "clang" in (out.stdout + out.stderr).lower()


def c_compiler():
    cc = os.environ.get("CC")
    if cc:
        return cc
    # macOS /usr/bin/gcc is Apple clang, which rejects the generated
    # files' implicit declarations. Prefer a real GNU gcc, including
    # brew's gcc-N, which is what the macOS CI build uses.
    for major in range(20, 10, -1):
        found = shutil.which("gcc-%d" % major)
        if found and not _compiler_is_clang(found):
            return found
    for name in ("gcc", "cc", "clang", "cl"):
        found = shutil.which(name)
        if not found:
            continue
        if name in ("gcc", "cc") and _compiler_is_clang(found):
            continue
        return found
    for name in ("gcc", "cc", "clang"):
        found = shutil.which(name)
        if found:
            return found
    raise SystemExit("no C compiler on PATH; set CC")


def _is_cl(cc):
    return Path(cc).name.lower() in ("cl", "cl.exe")


def sphere_binary_name():
    return "sphere.exe" if os.name == "nt" else "sphere"


def _first_present(work, names):
    for name in names:
        if (work / name).is_file():
            return name
    return None


def model_sources(work):
    """Unix sdfast writes sphere_dyn.c / sphere_sar.c. _WIN32, including
    MinGW and MSVC, writes the short names sphere_d.c / sphere_s.c.
    """
    work = Path(work)
    dyn = _first_present(work, ("sphere_dyn.c", "sphere_d.c"))
    sar = _first_present(work, ("sphere_sar.c", "sphere_s.c"))
    lib = _first_present(work, ("sdlib.c",))
    missing = []
    if dyn is None:
        missing.append("sphere_dyn.c or sphere_d.c")
    if sar is None:
        missing.append("sphere_sar.c or sphere_s.c")
    if lib is None:
        missing.append("sdlib.c")
    if missing:
        present = sorted(p.name for p in work.iterdir())
        raise SystemExit(
            "sdfast did not write %s (directory has: %s)"
            % (", ".join(missing), " ".join(present))
        )
    return ["sphere.c", dyn, sar, lib]


def compile_commands(cc, exe, sources):
    """sphere.c is C17. Generated files stay gnu89: they still call
    each other without prototypes.
    """
    driver, generated = sources[0], sources[1:]
    if _is_cl(cc):
        objs = ["sphere.obj"]
        commands = [[
            cc, "/nologo", "/std:c17", "/W4", "/WX", "/TC", "/c",
            "/Fosphere.obj", driver,
        ]]
        for src in generated:
            obj = Path(src).stem + ".obj"
            objs.append(obj)
            commands.append([cc, "/nologo", "/TC", "/c", src])
        commands.append([cc, "/nologo", "/Fe:" + str(exe), *objs])
        return commands
    commands = [[
        cc, "-std=c17", "-Wall", "-Wextra", "-Wpedantic", "-Werror",
        "-c", driver, "-o", "sphere.o",
    ]]
    objs = ["sphere.o"]
    for src in generated:
        obj = Path(src).stem + ".o"
        objs.append(obj)
        commands.append([
            cc, "-std=gnu89",
            "-Wno-error=implicit-function-declaration",
            "-Wno-error=implicit-int",
            "-c", src, "-o", obj,
        ])
    commands.append([cc, "-o", str(exe), *objs, "-lm"])
    return commands


def case_stdin(name):
    values = CASES[name]
    # mu_s, mu_d, restitution, rad radv, about-z, about-x, wx wy wz
    lines = [
        "%g" % values[0],
        "%g" % values[1],
        "%g" % values[2],
        "%g %g" % values[3:5],
        "%g %g" % values[5:7],
        "%g %g" % values[7:9],
        "%g %g %g" % values[9:12],
        "",
    ]
    return "\n".join(lines)


def generate_and_run(sdfast, case, work):
    work = Path(work)
    work.mkdir(parents=True, exist_ok=True)
    for name in ("sphere.c", "sphere.sd"):
        shutil.copyfile(HERE / name, work / name)
    sdfast = Path(sdfast).resolve()
    generated = subprocess.run(
        [str(sdfast), "-lc", "-ge", "sphere.sd"],
        cwd=str(work),
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    try:
        sources = model_sources(work)
    except SystemExit:
        sys.stderr.write(generated.stderr)
        sys.stderr.write(generated.stdout)
        raise
    if generated.returncode != 0:
        sys.stderr.write(generated.stderr)
        sys.stderr.write(generated.stdout)
        raise SystemExit("sdfast exited %s" % generated.returncode)
    cc = c_compiler()
    exe = work / sphere_binary_name()
    for command in compile_commands(cc, exe, sources):
        subprocess.run(command, cwd=str(work), check=True)
    completed = subprocess.run(
        [str(exe)],
        input=case_stdin(case),
        text=True,
        cwd=str(work),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if completed.returncode != 0:
        sys.stderr.write(completed.stderr)
        sys.stderr.write(completed.stdout)
        raise SystemExit("sphere exited %s" % completed.returncode)
    data = work / "sphere_anim.dat"
    if not data.is_file():
        raise SystemExit("sphere did not write sphere_anim.dat")
    return data


def _anim_rows(path):
    rows = []
    with open(path, "r", encoding="utf-8", errors="replace") as handle:
        for raw in handle:
            parts = raw.split()
            if len(parts) < 15 or parts[0] == "t":
                continue
            rows.append(parts)
    if len(rows) < 2:
        raise SystemExit("no samples in %s" % path)
    return rows


def summarize_anim(path):
    """Stdlib-only check used by --no-render. Does not import numpy."""
    rows = _anim_rows(path)
    return len(rows), rows[0][0], rows[-1][0], rows[0][2]


def load_anim(path):
    rows = _anim_rows(path)
    import numpy as np
    times = [float(row[0]) for row in rows]
    cond = [row[2] for row in rows]
    pos = [[float(row[3]), float(row[4]), float(row[5])] for row in rows]
    rots = [[float(item) for item in row[6:15]] for row in rows]
    return (
        np.asarray(times),
        np.asarray(pos),
        np.asarray(rots).reshape(-1, 3, 3),
        cond,
    )


def _mesh(radius, n):
    import numpy as np
    u = np.linspace(0, 2 * np.pi, n)
    v = np.linspace(0, np.pi, max(n // 2, 8))
    x = radius * np.outer(np.cos(u), np.sin(v))
    y = radius * np.outer(np.sin(u), np.sin(v))
    z = radius * np.outer(np.ones_like(u), np.cos(v))
    return x, y, z


def _curves():
    import numpy as np
    phi = np.linspace(0, 2 * np.pi, 72)
    meridians = []
    for i, az in enumerate(np.linspace(0, np.pi, 6, endpoint=False)):
        meridians.append((
            np.column_stack([
                np.sin(phi) * np.cos(az),
                np.sin(phi) * np.sin(az),
                np.cos(phi),
            ]),
            "#d62728" if i == 0 else "#333333",
            2.0 if i == 0 else 0.7,
        ))
    latitudes = []
    for th in (0.45, 0.9, 2.24, 2.69):
        latitudes.append(np.column_stack([
            np.sin(th) * np.cos(phi),
            np.sin(th) * np.sin(phi),
            np.full_like(phi, np.cos(th)),
        ]))
    return meridians, latitudes


def _place(rot, pts, center):
    world = (rot @ pts.T).T + center
    return world[:, 0], world[:, 2], world[:, 1]


def _place_grid(rot, grids, center):
    import numpy as np
    shape = grids[0].shape
    flat = np.stack(grids, axis=0).reshape(3, -1)
    world = rot @ flat
    out = [world[i].reshape(shape) + center[i] for i in range(3)]
    return out[0], out[2], out[1]


def draw(ax, times, pos, rots, cond, index, title, shell, ball):
    ax.cla()
    ax.plot_surface(*_place_grid(np_eye(), shell, (0, 0, 0)),
                    color="#bbbbbb", alpha=0.08, linewidth=0)
    center = pos[index]
    rot = rots[index]
    ax.plot_surface(*_place_grid(rot, ball, center),
                    color="#9ecae1", alpha=0.18, linewidth=0)
    meridians, latitudes = _curves()
    for pts, color, width in meridians:
        ax.plot(*_place(rot, pts, center), color=color, linewidth=width)
    for pts in latitudes:
        ax.plot(*_place(rot, pts, center), color="#444444", linewidth=0.6)
    end = index + 1
    colors = {"FREE": "#2ca02c", "ROLLING": "#1f77b4", "SLIPPING": "#d62728"}
    for j in range(1, end):
        ax.plot(
            pos[j - 1:j + 1, 0], pos[j - 1:j + 1, 2], pos[j - 1:j + 1, 1],
            color=colors.get(cond[j], "#333333"), linewidth=1.2,
        )
    lim = 2.4
    ax.set_xlim(-lim, lim)
    ax.set_ylim(-lim, lim)
    ax.set_zlim(-lim, lim)
    # Default mplot3d box is wider than tall, which draws the balls oblate.
    ax.set_box_aspect((1, 1, 1))
    ax.set_xlabel("x")
    ax.set_ylabel("z")
    ax.set_zlabel("y")
    ax.set_title("%s  t=%.2fs  %s" % (title, times[index], cond[index]))
    ax.view_init(elev=18, azim=-58)


def np_eye():
    import numpy as np
    return np.eye(3)


def _frame_ids(times, until, stride):
    ids = [i for i, t in enumerate(times) if until is None or t <= until + 1e-9]
    if len(ids) < 2:
        ids = list(range(len(times)))
    return ids[::max(stride, 1)]


def save_still(path, bundle, when, title):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import numpy as np
    times, pos, rots, cond = bundle
    index = int(np.argmin(np.abs(times - when)))
    shell, ball = _mesh_pair()
    fig = plt.figure(figsize=(5, 5))
    ax = fig.add_subplot(111, projection="3d")
    draw(ax, times, pos, rots, cond, index, title, shell, ball)
    fig.tight_layout()
    fig.savefig(path, dpi=110)
    plt.close(fig)


def _mesh_pair():
    return _mesh(2.0, 28), _mesh(1.0, 22)


def save_movie(path, bundle, until, stride, fps, title):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.animation import FFMpegWriter, FuncAnimation, PillowWriter
    import shutil as _shutil
    times, pos, rots, cond = bundle
    frames = _frame_ids(times, until, stride)
    shell, ball = _mesh_pair()
    fig = plt.figure(figsize=(5, 5))
    ax = fig.add_subplot(111, projection="3d")

    def update(frame_index):
        draw(ax, times, pos, rots, cond, frame_index, title, shell, ball)
        return ()

    anim = FuncAnimation(fig, update, frames=frames, interval=1000.0 / fps)
    if str(path).lower().endswith(".gif") or _shutil.which("ffmpeg") is None:
        if not str(path).lower().endswith(".gif"):
            path = Path(str(path).rsplit(".", 1)[0] + ".gif")
        anim.save(path, writer=PillowWriter(fps=fps))
    else:
        anim.save(path, writer=FFMpegWriter(fps=fps, bitrate=900))
    plt.close(fig)
    return Path(path)


def self_test():
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        single = root / "single"
        both = root / "both"
        multi = root / "multi"
        for directory, names in (
            (single, ("sdfast",)),
            (both, ("sdfast", "Release/sdfast.exe")),
            (multi, ("Release/sdfast.exe",)),
        ):
            for name in names:
                path = directory / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("x", encoding="ascii")
        assert search_build_tree(single).name == "sdfast"
        assert search_build_tree(both) == both / "sdfast"
        found = search_build_tree(multi)
        assert found == multi / "Release" / "sdfast.exe", found
        spaced = root / "my build"
        (spaced / "RelWithDebInfo").mkdir(parents=True)
        exe = spaced / "RelWithDebInfo" / "sdfast.exe"
        exe.write_text("x", encoding="ascii")
        assert search_build_tree(spaced) == exe
        unix = root / "unix-names"
        win = root / "win-names"
        for directory, names in (
            (unix, ("sphere_dyn.c", "sphere_sar.c", "sdlib.c")),
            (win, ("sphere_d.c", "sphere_s.c", "sdlib.c")),
        ):
            directory.mkdir()
            for name in names:
                (directory / name).write_text("x", encoding="ascii")
        assert model_sources(unix) == [
            "sphere.c", "sphere_dyn.c", "sphere_sar.c", "sdlib.c"]
        assert model_sources(win) == [
            "sphere.c", "sphere_d.c", "sphere_s.c", "sdlib.c"]
        saved = os.environ.pop("SDFAST", None)
        cwd = Path.cwd()
        try:
            os.chdir(root)
            found = find_sdfast("single", None)
            assert found.is_absolute(), found
            assert found == (single / "sdfast").resolve()
        finally:
            os.chdir(cwd)
            if saved is not None:
                os.environ["SDFAST"] = saved
        sample = root / "anim.dat"
        sample.write_text(
            "t mark cond cx cy cz r00 r01 r02 r10 r11 r12 r20 r21 r22\n"
            "0 0 FREE 0 0 0 1 0 0 0 1 0 0 0 1\n"
            "0.02 0 FREE 0 -0.002 0 1 0 0 0 1 0 0 0 1\n",
            encoding="ascii",
        )
        assert summarize_anim(sample) == (2, "0", "0.02", "FREE")
    print("self-test ok")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--case", choices=sorted(CASES), default="xspin")
    parser.add_argument("--build-dir", help="CMake build directory to search")
    parser.add_argument("--sdfast", help="path to the sdfast executable")
    parser.add_argument("--output", default="sphere.mp4")
    parser.add_argument("--still", help="also write one PNG")
    parser.add_argument("--still-time", type=float, default=0.48)
    parser.add_argument("--until", type=float, default=2.0)
    parser.add_argument("--stride", type=int, default=2)
    parser.add_argument("--fps", type=int, default=12)
    parser.add_argument("--no-render", action="store_true")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()
    if args.self_test:
        self_test()
        return 0
    sdfast = find_sdfast(args.build_dir, args.sdfast)
    print("using sdfast %s" % sdfast)
    with tempfile.TemporaryDirectory(prefix="sphere-anim-") as tmp:
        data = generate_and_run(sdfast, args.case, tmp)
        if args.no_render:
            count, t0, t1, cond0 = summarize_anim(data)
            print("%s samples, t=%s..%s, first cond %s" % (count, t0, t1, cond0))
            return 0
        rows = load_anim(data)
    print("%s samples, t=%.3f..%.3f, first cond %s" % (
        len(rows[0]), rows[0][0], rows[0][-1], rows[3][0]))
    written = save_movie(
        args.output, rows, args.until, args.stride, args.fps, args.case)
    print("wrote %s" % written)
    if args.still:
        save_still(args.still, rows, args.still_time, args.case)
        print("wrote %s" % args.still)
    return 0


if __name__ == "__main__":
    sys.exit(main())
