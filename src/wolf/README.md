# Wolfenstein-like PLTC/HCØ Experience

This program is a Standard ML port (and a modification) of Jacob Seidelin's
JavaScript implementation of a Wolfenstein 3D-like game using raycasting [3,4] and
DOM-manipulations [1,2]. The Standard ML code is translated into JavaScript
using SmlToJs [5].

## Assumptions

To compile the sources, you need to install [MLKit](https://github.com/melsman/mlkit), which includes the SmlToJs compiler `smltojs`.

## Compilation

Assuming `smltojs` is installed, simply run `make`:

```
  $ make
  ...
  [Created file run.html]
```

To start the program in a browser, fetch `run.html`. You should now see the following screen:

![Screen-dump](screendump.png)

You navigate using keyboard arrows. There is currently no game play. Just walk
around and look at the different professors...

## License

MIT-License.

## Resources

[1] https://web.archive.org/web/20121220071947/http://dev.opera.com/articles/view/creating-pseudo-3d-games-with-html-5-can-1

[2] https://web.archive.org/web/20121222001735/http://dev.opera.com/articles/view/3d-games-with-canvas-and-raycasting-part-2

[3] https://lodev.org/cgtutor/raycasting.html

[4] https://lodev.org/cgtutor/raycasting2.html

[5] Martin Elsman. SMLtoJs: Hosting a Standard ML Compiler in a Web Browser. In
ACM SIGPLAN 2011 International Workshop on Programming Language And Systems
Technologies for Internet Clients (PLASTIC ‘11). Portland, Oregon,
USA. October, 2011. [pdf](https://elsman.com/pdf/smltojs-final.pdf).
