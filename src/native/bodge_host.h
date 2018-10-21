#ifndef _BODGE_HOST_H_
#define _BODGE_HOST_H_

#if defined(_WIN32)
#elif defined(__APPLE__)
#else
#  include <X11/Xlib.h>
#  include <X11/Xutil.h>
#endif

#endif /* _BODGE_HOST_H_ */
