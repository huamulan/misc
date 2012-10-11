
打开一个网址：
adb shell am start http://news.sohu.com

打开一个u盘上的video：
adb shell am start -a android.intent.action.VIEW -t 'video/*' -d file:///mnt/sdcard/1080P_The.World.of.Pandora.mov

打开一个远程video：
adb shell am start -a android.intent.action.VIEW -t 'video/*' -d 'http://218.60.38.58:80/work/71972/119/9.mp4?xx=yy'

打开一个audio文件
adb shell am start -a android.intent.action.VIEW -t 'audio/*' -d file:///mnt/sdcard/009.mp3

m3u8 streaming video
adb shell am start -a android.intent.action.VIEW -t 'video/*' -d 'http://meta.video.qiyi.com/137/be58e12d59aa6deedc342f198837a375.m3u8'
