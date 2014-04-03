#include <stdio.h>
#include <X/Xlib.h>

main()
{
    Color color;
    int i;

    XOpenDisplay(NULL);
    for (i = 0; i < 256; i++)
    {
	color.pixel = i;
	if (XQueryColor(&color))
	    printf("#%x\t%5d%5d%5d\n", i,
		color.red / 256, color.green / 256, color.blue / 256);
    }
}
