#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include <utils/Log.h>

static void cat_ion_info(void) {
    LOGD("## CAT_INO_INFO ##");
    int fd = -1;
    char * path = "/sys/kernel/debug/ion/carveout_heap";

    if((fd = open(path, O_RDONLY)) < 0) {
        LOGE("open file failed errno=%d\n",errno);
    } else {
        int n = 1024*10;
        char *tmp = new char[n];
        memset(tmp, 0, n);
        char *p = tmp, *p1 = tmp;

        if (read(fd, tmp, n) > 0) {
            LOGW("info: %s\n\n", tmp);
            for(;;) {
                p1 = strchr(p,'\n');
                if (p1 == NULL) {
                    LOGD("%s",p);
                    break;
                }
                *p1 = '\0';
                LOGD("%s\n",p);
                p = p1+1;
            }
        }
        delete [] tmp;
        tmp = NULL;
        close(fd);
    }
}

int main() {
    cat_ion_info();
    return 0;
}
