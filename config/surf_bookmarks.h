/*

   Excerpt from my config.h:

*/

/*
 
    Things I would eventually like to do to improve the system:

     1. Give priority to bookmarks I visit often and created recently.
        This is probably also one of the most complicated fixes and I
        haven't given it much thought. I would like to make it happen
        some rainy day. Many of my bookmarks are just things I save to
        look at the next day or whatever, so I want those to bubble
        down to the bottom of the list as I never access them again,
        yet I want them to appear at the top when I created them
        recently.

     2. Allow retagging of bookmarks. A fairly simple fix that would
        mean a world of difference. I currently retag by editing them
        manually in Vim...

     3. Deleting bookmarks. Not that I would need this with priority
        given to frequently visited bookmarks, but it's a feature that
        sounds too obvious not to have.

     4. Non-local storage of bookmarks. A friend has made a small
        adaption to my system to allow him to have his bookmarks
        centralised on a server. It apparently works really well for
        him, but it's not something I need so I haven't cared much
        for it.

    The actual system currently:

*/

// ...

#define BM_PICK { .v = (char *[]){ "/bin/sh", "-c",  "xprop -id $0 -f _SURF_GO 8s -set _SURF_GO " \
    "`cat ~/.surf/bookmarks | dmenu -l 5 | cut -d ' ' -f 1" \
    "|| exit 0`", winid, NULL } }

#define BM_ADD { .v = (char *[]){ "/bin/sh", "-c", "(echo -n `xprop -id $0 _SURF_URI |" \
    "cut -d '\"' -f 2` | dmenu | grep '://') >> ~/.surf/bookmarks", winid, NULL } }

// ...

    { MODKEY,               GDK_t,      spawn,      BM_PICK },
    { MODKEY,               GDK_s,      spawn,      BM_ADD },

// ...
