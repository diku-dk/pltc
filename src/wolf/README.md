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

## Notes and Design Aspects

- Support slide shows by using walls of type 5 and 6 next to each
  other.

  Slides are organised in a folder structure, as follows:

  ```
  characters/
    characters.json:["athas","mael",...]
    athas/
	  character.json:{name:"Troels",avatar:"professor1.gif",office:"01-0-S14"}
	  slides.json:["array-21"]
	  array21/
	    meta.json: {pages:"23", voice:"DK"}
		array21.pdf
		array21-001.png ...        // slide pages to be viewed
		array21-023.png
		array21-001.txt            // text to be spoken
  ```

  Based on the json-files, we can learn about what files are available and the
  proper slides can be shown and the proper txt-file can be spoken (with the
  proper audio).

  The player moves around and interacts with screens, which show
  presentations. There is a map from screens to presentation objects, where each
  presentation has a "current page" property.

  When a player is close to a screen, the player may select (by pressing `s`)
  among slide shows associated with the character who owns the screen. There is
  a mapping from locations to pairs of a screen and an owner.

  When a player is close to a screen, the player may also advance or rewind the
  current presentation by pressing `n` (for next) or `p` (for prev). When the
  current page changes, the current speech synthesis is cancelled
  (`window.speechSynthesis.cancel()`) and the speech synthesis for the new
  current page is started.

  Multi-page pdfs of size 16:9 may be split up into multiple pngs, using
  the following ImageMagick command:

  ```
  $ magick -density 288 datoek.pdf -background white -alpha background -alpha off -resize 75% datoek-%03d.png
  ```

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
