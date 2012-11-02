LOCAL_PATH:= $(call my-dir)
include $(CLEAR_VARS)

LOCAL_MODULE_TAGS := optional

#LOCAL_ARM_MODE := arm

LOCAL_SRC_FILES := \
	gsnap.c

LOCAL_C_INCLUDES += \
	$(TOP)/external/jpeg \
	$(TOP)/external/libpng \
	$(TOP)/external/zlib

LOCAL_SHARED_LIBRARIES:= \
	libjpeg \
	libz \

LOCAL_STATIC_LIBRARIES:= \
	libpng

LOCAL_MODULE:= gsnap

include $(BUILD_EXECUTABLE)
