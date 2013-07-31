//#include <cv.h>
//#include <highgui.h>

#include <stdio.h>
#include <opencv2/opencv.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/highgui/highgui_c.h>

int main(int argc, char const *argv[])
{
    cvNamedWindow("Window", 1);
    IplImage *img = cvCreateImage(cvSize(640, 480), IPL_DEPTH_8U, 1);
    CvFont font;
    double hScale = 1.0;
    double vScale = 1.0;
    int lineWidth = 1;

    cvInitFond(&font, CV_FONT_HERSHEY_SIMPLEX | CV_FONT_ITALIC, hScale, vScale, 0, lineWidth);
    cvPutText( img, "Hello World!", cvPint(200, 400), &font, cvScalar( 255, 255, 0 ) );
    cvShowImage("Window", img);
    cvWaitKey();
    return 0;
}
