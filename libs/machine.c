/* Copyright 2026 Michael Sherman
 * Copyright 1989-2025 PTC Inc.; 1984-1988 Symbolic Dynamics, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*==========================================================================*/
/*                     MACHINE DEPENDENT ROUTINES                           */
/*==========================================================================*/

#ifdef vms
#include <errno.h>
#include <common/src/include/types.h>
#include <time.h>
#include <descrip.h>
#else
#if defined(RHP)
#include <sys/times.h>
#include <limits.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <netio.h>
#else
#ifdef RWIN32
#include <time.h>
#include "rainbow.h"
#else
#if defined(RSUN5) || defined(RNEC_MIPS)
#include <sys/times.h>
#include <sys/systeminfo.h>
#endif
#include <sys/errno.h>
#include <sys/types.h>
// #include <sys/time.h>
#include <time.h>
#include <sys/resource.h>
#endif
#endif
#endif

#include "libs.h"
#include "libsprot.h"

#ifdef _AIX
#include <time.h>
#include <sys/utsname.h>
#endif 

#if defined(ultrix) || defined(RALPHA)
#  include <strings.h>
#  include <sys/socket.h>
#  include <sys/ioctl.h>
#  include <net/if.h>
#  ifndef IFF_DYNPROTO
#    define IFF_DYNPROTO 0x0 
#  endif
#  ifdef vax
#    include <netinet/if_ether.h>
#  else
#    include <netinet/in.h>
#  endif
#  define IFREQCNT        64
#endif

time_t time();

/*===================================*/
/*            GETDATE                */
/* Return date in format DD-MON-YYYY */
/* DD is ' D' if day < 10.           */
/*===================================*/

void GETDATE(register string11 s)
{
    time_t clock;
    register char *t;

    time(&clock);
    t = ctime(&clock);
    s[0] = t[8];
    s[1] = t[9];
    s[2] = '-';
    s[3] = t[4];
    s[4] = t[5];
    s[5] = t[6];
    s[6] = '-';
    s[7] = t[20];
    s[8] = t[21];
    s[9] = t[22];
    s[10] = t[23];
    s[11] = '\0';
}

/*=====================================*/
/*              GETTIME                */
/* Return time in format "hh:mm:ss"    */
/*=====================================*/

void GETTIME(string11 s)
{
    time_t clock;
    char *t;

    time(&clock);
    t = ctime(&clock);
    strncpy(s, t + 11, 8);
    s[8] = '\0';
}

/* GETNUMTIME
 *
 * Get time as an integer hhmmss, e.g. 142512 means
 * "25 minutes and 12 seconds after 2 pm".
 */
long
GETNUMTIME(void)
{
    struct tm *tm;
    time_t clock;

    time(&clock);
    tm = localtime(&clock);
    return tm->tm_hour*10000L + tm->tm_min*100L + tm->tm_sec;
}

/* GETNUMDATE
 *
 * Return today's date as a number yyyymmdd like 19891101.            
 * Used for checking expiration date against that returned by
 * DECRYPTFILE.
 */
long 
GETNUMDATE(void)
{
    struct tm *tm;
    time_t clock;

    time(&clock);
    tm = localtime(&clock);
    return (tm->tm_year+1900)*10000L + (tm->tm_mon+1)*100L + tm->tm_mday;
}

/*========================================*/
/*                openr                   */
/* Open an existing file for read access. */
/* Return 1 if we fail, else 0.           */
/*========================================*/

int
openr(FILE **f,
      char *fn)
{
    if (!(*f = fopen(fn, "r"))) {
        perror(fn);
        return 1;
    }
    return 0;
}

/*===================================*/
/*               openw               */
/* Open a file for write access and  */
/* create it if necessary.           */
/* Return 1 if we fail, else 0.      */
/*===================================*/

int
openw(FILE **f,
      char *fn)
{
    if (!(*f = fopen(fn, "w"))) {
        perror(fn);
        return 1;
    }
    return 0;
}

/*==============================*/
/*      CLOSE_FILE              */
/* Close a file.                */
/* Return 1 if we fail, else 0. */
/*==============================*/

int CLOSE_FILE(FILE *f)
{
    if (fclose(f)) {
        perror("close");
        return 1;
    }
    return 0;
}

/*=================================================*/
/*                 CPU_SECONDS                     */
/* Returns the number of CPU seconds               */
/* since some unspecified time in the past.        */
/* Should be called once at start of program, then */
/* all later times should be taken relative to     */
/* the start time.                                 */
/*=================================================*/

double CPU_SECONDS(void)
{
#ifdef RWIN32
    return clock() / (double)CLOCKS_PER_SEC;
#else 
#ifdef vms
    struct tbuffer tbuf;

    times(&tbuf);
    return tbuf.proc_user_time/100.;
#else
#if defined(RHP) || defined(RSUN5) || defined(RNEC_MIPS)
    struct tms tbuf;

    times(&tbuf);
    return ((double)tbuf.tms_utime)/CLK_TCK;
#else
    struct rusage rusage;

    if (getrusage(RUSAGE_SELF, &rusage)) {
#ifndef apollo /* apollo appears to screw up the error return */
        perror("getrusage");
        exit(4);
#endif
    }
    return rusage.ru_utime.tv_sec + rusage.ru_utime.tv_usec / 1000000.;
#endif
#endif
#endif
}

/* GETMACHINEID
 *   Return this machine's machine ID as best we can.           
 *   Used for checking machine ID against that returned by      
 *   DECRYPTFILE.                                               
 *     On the Sun or Sony, we return exactly what hostid(1) returns,    
 *   that is, the gethostid value in hex.                       
 *     On a DEC machine running Ultrix, we read the ethernet
 *   hardware ID from device "se0" (for RISC machines) or device
 *   "qe0" (for VAXen).  This is a kludge -- really we should 
 *   try to determine the actual device name.
 *     On a VAX running VMS, we return the hardware "SID" (8 hex chars)   
 *   followed by the first 6 chars of the nodename, with blank  
 *   padding if necessary.  Any illegal chars in the nodename   
 *   are replaced with '_'.   The nodename used is the cluster  
 *   name (what you get from f$getsyi("nodename")), unless that 
 *   returns a 0-length name in which case we'll try the decnet 
 *   node name from logical name SYS$NODE.  In SYS$NODE the     
 *   node name is followed by "::", which we remove. If there   
 *   is no SYS$NODE either, we'll return a null nodename, i.e., 
 *   machID is just the SID.                                    
 *     Check below for other machines.  If all else fails,
 *     we just return 'unknown'.  
 */
void GETMACHINEID(string20 machID)
{
#ifdef RSUN5
    char                buf[128];
    unsigned int        rhid;

    sysinfo(SI_HW_SERIAL, buf, sizeof( buf ) );
    sscanf( buf, "%d", &rhid );
    sprintf(machID, "%08x", rhid);
#else
#ifdef RNEC_MIPS
    char                buf[128];
    unsigned int        rhid;

    sysinfo(SI_HW_SERIAL, buf, sizeof( buf ) );
    sscanf( buf, "%x", &rhid );
    sprintf(machID, "%08x", rhid);
#else
#ifdef sun
    long gethostid();

    sprintf(machID, "%08x", gethostid());
#else
#ifdef sony
    long gethostid();

    sprintf(machID, "%08x", gethostid());
#else
#ifdef sgi
    long sysid();

    sprintf(machID, "%08x", sysid(0));
#else
#ifdef apollo
  long uid[2];

  proc2_$who_am_i(uid);
  sprintf(machID, "%x", uid[1] & 0xfffff);
#else
#ifdef RHP
    int                        i, fd;
    struct fis                arg;
    unsigned long        id=0;


    fd = open( "/dev/lan0", O_RDONLY|O_NDELAY );

    if (fd != -1) {
        arg.reqtype = LOCAL_ADDRESS;
        if( ioctl( fd, NETSTAT, &arg ) != 0 ) {
            perror("ioctl (NETSTAT)");
            sprintf(machID, "unknown");
            return;
        }

        close( fd );

        for (i=2; i<6; i++)
            id = (id << 8) | (0xff & arg.value.s[i]);
    } else {
        char        buf[100];
        FILE        *f;
        f = popen( "/etc/lanscan | grep lan0 | awk '{print $2}'", "r" );
        if( ! f ) {
            perror("/etc/lanscan");
            sprintf(machID, "unknown");
            return;
        }

        if( fgets( buf, sizeof buf, f ) ) {
            sscanf( &buf[6], "%x", &id );
        }
        else {
            perror("open(/dev/lan0)");
            sprintf(machID, "unknown");
            pclose(f);
            return;
        }
        pclose(f);
    }

    sprintf(machID, "%08x", id);
#else 
#ifdef _AIX
    long nid;
    struct xutsname un;

    unamex( &un );
    sprintf(machID, "%08x", (unsigned long)un.nid);
#else
#if defined(ultrix) || defined(RALPHA)
    struct ifreq        *ifr;
    struct ifreq        ifreqs[IFREQCNT];
    struct ifreq        tmp_ifr;
    struct ifconf        ifc;
    struct ifdevea        ifrp;
    int                        i, s;
    unsigned long         id = 0;

    if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perror("socket");
        sprintf(machID, "unknown");
        return;
    }

    ifc.ifc_req = ifreqs;
    ifc.ifc_len = sizeof(ifreqs);
    if (ioctl(s, SIOCGIFCONF, (caddr_t)&ifc) < 0) {
        perror("ioctl (SIOCGIFCONF)");
        sprintf(machID, "unknown");
        return;
    }

    errno = 0;
    /*
     * Loop through list of possible network interfaces
     */
    for (ifr = ifreqs; ifr < &ifreqs[IFREQCNT-1]; ifr++) {
        if (strlen(ifr->ifr_name) == 0) {
            fprintf(stderr, "No network devices configured\n");
            sprintf(machID, "unknown");
            return;
        }

        (void) strcpy(tmp_ifr.ifr_name, ifr->ifr_name);
        if (ioctl(s, SIOCGIFFLAGS, &tmp_ifr) < 0) {
            perror("ioctl(SIOCGIFFLAGS)");
            sprintf(machID, "unknown");
            return;
        }

        /*
         * skip devices which aren't up and running, etc
         */
        if (((tmp_ifr.ifr_flags & (IFF_UP|IFF_RUNNING)) != 
                    (IFF_UP|IFF_RUNNING)) ||
            ((tmp_ifr.ifr_flags & (IFF_POINTOPOINT)) ==
                    (IFF_POINTOPOINT)) ||
            ((tmp_ifr.ifr_flags & (IFF_DYNPROTO|IFF_BROADCAST)) !=
                    (IFF_DYNPROTO|IFF_BROADCAST))) {

            continue; /* skip this one */
        }
        /*
         * found a valid Ethernet interface
         */
        (void) strcpy(ifrp.ifr_name, tmp_ifr.ifr_name);
        break;
    }

    /* 
     * read the physical address
     */
    if (ioctl(s, SIOCRPHYSADDR, &ifrp) < 0) {
        perror("ioctl(SIOCRPHYSADDR)");
        sprintf(machID, "unknown");
        return;
    }

    close(s);
 
    for (i=2; i<6; i++)
        id = (id << 8) | (0xff & ifrp.default_pa[i]);

    sprintf(machID, "%08x", id);
#else
#ifdef vms
#define ss$_normal    1
#define syi$_sid      4097
#define syi$_nodename 4313
#define lnm$_string   2

    typedef struct {
        short buflen;
        short code;
        union {
            struct {
                long *u1_intadr;
                short *u1_intlen;
            } u1;
            struct {
                char *u2_stradr;
                short *u2_strlen;
            } u2;
        } u;
    } item_t;

#define intadr u.u1.u1_intadr
#define intlen u.u1.u1_intlen
#define stradr u.u2.u2_stradr
#define strlen u.u2.u2_strlen

    typedef item_t ilst_t[3];

    typedef struct {
        short namlen;
        short namcod;
        char *namadr;
        short *retadr;
    } trnitem_t;

    typedef trnitem_t trnlst_t[2];

    ilst_t itmlst;
    trnlst_t trn;
    long status,len,i;
    char nodenm[20];
    long sid;
    short sidlen,nodenmlen;

    extern long sys$getsyiw();
    extern long sys$trnlnm();

    itmlst[0].code = syi$_sid;
    itmlst[0].buflen = 4;
    itmlst[0].intadr = &sid;
    itmlst[0].intlen = &sidlen;
    itmlst[1].code = syi$_nodename;
    itmlst[1].buflen = 15;
    itmlst[1].stradr = nodenm;
    itmlst[1].strlen = &nodenmlen;
    itmlst[2].code = 0;
    itmlst[2].buflen = 0;

    status = sys$getsyiw(0,0,0,itmlst,0,0,0);
    if (status != ss$_normal) {
        sprintf(machID, "unknown");
        return;
    }
    sprintf(machID, "%08x", sid);
    if (nodenmlen <= 0) {
        $DESCRIPTOR(systab,"LNM$SYSTEM");
        $DESCRIPTOR(sysnode,"SYS$NODE");
        trn[0].namlen = 15;
        trn[0].namcod = lnm$_string;
        trn[0].namadr = nodenm;
        trn[0].retadr = &nodenmlen;
        trn[1].namlen = 0;
        trn[1].namcod = 0;
        status = sys$trnlnm(0,&systab,&sysnode,0, trn);
        if ((status != ss$_normal) || (nodenmlen <= 0)) {
            nodenm[0] = ':';
            nodenm[1] = ':';
            nodenmlen = 2;
        }
    }
    /* strip off trailing `:'s */
    while (nodenmlen > 0 && nodenm[nodenmlen - 1] == ':')
        nodenmlen--;
    if (nodenmlen > 6) 
        len = 6;
    else len = nodenmlen;
    for (i = 0; i < len; i++) 
        if ((CH2INT(nodenm[i]) < 0) || (nodenm[i] == '$'))
            machID[i+8] = '_';
        else machID[i+8] = nodenm[i];
    machID[i+8] = '\0';
#else
#ifdef RWIN32
    strcpy(machID, "unknown");
    return;
#else
#ifdef ardent        /* Stardent Computers, Inc. */
#   include <sys/sysmips.h>
    /*
     * get the prom id of the boot cpu
     */
    union {
            char idbuf[128];
            short intbuf[32];
    }   id;
    int boot_cpu;
    int off;

    /*
     * first get the cpuid of the boot cpu 
     */
    boot_cpu = sysmips(SBOOT_CPU,0);

    /*
     * now get the id of the boot cpu.  Each cpu contains 32 bytes of
     * identifier.  There is a maximum of 4 cpus.  Allegedly, the uniq 
     * cpu id is the third short in the 32 byte field of the boot cpu.
     */
    sysmips(SREAD_IDPROM,id.idbuf);
    off = boot_cpu * 16 + 2;
    sprintf(machID, "%08x", (unsigned long)id.intbuf[off]);

#else
#ifdef mips /* a MIPS workstation, from MIPS Inc. */
#    define imp_tbl bsd43_imp_tbl
#    include <machine/hwconf.h>
#    undef imp_tbl

    struct hw_config Conf;

    if (hwconf(HWCONF_GET, &Conf) < 0) {
        strcpy(machID, "unknown");
        return;
    }

    sprintf(machID,"%c%c%c%c%c", Conf.cpubd_snum[0],
            Conf.cpubd_snum[1], Conf.cpubd_snum[2],
            Conf.cpubd_snum[3], Conf.cpubd_snum[4]);

#else
#ifdef RNEC
    long gethostid();

    sprintf(machID, "%08x", gethostid());
#else
    strcpy(machID, "unknown");
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
}
