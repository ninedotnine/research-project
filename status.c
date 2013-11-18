/* 
 * 9.9
 * writes a handy status message to stdout
 * compile with: gcc -Wall -pedantic -std=c99 dwm-status.c
 * last updated aug 16 2013
 */

// see: feature_test_macros (7)
#define _BSD_SOURCE

/* SETTINGS */

// the format for the date/time
#define TIMESTRING "[%a %b %d] %H:%M"
// the format for everything
#define OUTFORMAT "[%.2f %.2f %.2f] [batt: %d%%] [mail %d] [pkg %d] [net %s] %s"
// level to warn on low battery
#define WARN_LOW_BATT 12
// files -- populated by cron
#define MAILFILE "/tmp/dwm-status.mail"
#define PKGFILE "/tmp/dwm-status.packages"
#define FBCMDFILE "/tmp/dwm-status.fbcmd"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <string.h>

static char *getdatetime(void) {
    char *buf;
    time_t result = time(NULL);
    struct tm *resulttm;

    if ((buf = malloc(sizeof(char)*65)) == NULL) {
        fprintf(stderr, "Cannot allocate memory for buf.\n");
        return "time ???";
    }
    result = time(NULL);
    resulttm = localtime(&result);
    if (resulttm == NULL) {
        fprintf(stderr, "Error getting localtime.\n");
        return "time ???";
    }
    if (! strftime(buf, sizeof(char)*65-1, TIMESTRING, resulttm)) {
        fprintf(stderr, "strftime is 0.\n");
        return "time ???";
    }
    return buf;
}

static int getfiledata(const char *filename) {
    // read an int from filename
    FILE *fd = fopen(filename, "r");
    if (fd == NULL) {
        fprintf(stderr, "error in getfiledata(): file not found: '%s'\n", filename);
        return -1;
    }
    int result;
    fscanf(fd, "%d", &result);
    fclose(fd);
    return result;
}

static int getbattery(void) {
    int capacity = getfiledata("/sys/class/power_supply/BAT0/capacity");

#ifdef zenity
    // display a warning on low battery and not plugged in. depends on zenity
    if (capacity <= WARN_LOW_BATT) {
        FILE *fd;
        fd = fopen("/sys/class/power_supply/BAT0/status", "r");
        if (fd == NULL) {
            fprintf(stderr, "error reading battery status.\n");
            return -1;
        }
        char *status;
        status = malloc(25);
        fscanf(fd, "%s", status);
        fclose(fd);
        if ( ! strncmp(status, "Discharging", 11) ) {
            fprintf(stderr, "issuing low battery warning\n");
            system("zenity --warning \
                    --text=\"charge my fuckin' battery!\""); 
        } 
    }
#endif
    return capacity;
}

static char *net(void) {
    // returns "ON" or "OFF"
    FILE *fp;
    fp = popen("ping -c 1 -W 1 google.com > /dev/null 2>&1 && \
               echo 'ON' || echo 'OFF'", "r");
    if (fp == NULL) {
        return "err";
    } 
    char *output = malloc(4);
    fgets(output, 4, fp);
    pclose(fp);

    // the last character might be an unwanted newline
    if (output[strlen(output)-1] == '\n') {
        output[strlen(output)-1] = '\0';
    }
    return output;
}

double *getavgs(void) {
    // load averages. must return exactly 3 doubles 
    double *avgs = malloc(3 * sizeof(double));
    int num_avgs = getloadavg(avgs, 3);
    if (num_avgs < 3) {
        if (num_avgs == -1) {
            fprintf(stderr, "num_avgs is -1");
            num_avgs = 0;
        } 
        // populate the rest of the array with meaningless doubles
        for (int i = num_avgs; i < 3; i++) {
            avgs[i] = 9.9;
        }
    }
    return avgs;
}


int main(void) {
    char *status;
    if ((status = malloc(80)) == NULL) {
        fprintf(stderr, "status = malloc(80) failed.\n");
        return EXIT_FAILURE;
    }
    
    double *avgs;
    // just run once and quit, don't loop for our purposes
    // for (; ; sleep(59)) {
    avgs = getavgs();
    snprintf(status, 90, OUTFORMAT, avgs[0], avgs[1], avgs[2],
             getbattery(), getfiledata(MAILFILE), 
             getfiledata(PKGFILE), net(), getdatetime());
    printf("%s\n", status);
    // }


    free(status);
    return 0;
}
