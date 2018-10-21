#ifndef _BODGE_HOST_H_
#define _BODGE_HOST_H_

#if defined(_WIN32)
#elif defined(__APPLE__)
#  include <ApplicationServices/ApplicationServices.h>
#else
#  define XUTIL_DEFINE_FUNCTIONS 1
#  include <X11/Xlib.h>
#  include <X11/Xutil.h>
#endif

#endif /* _BODGE_HOST_H_ */
