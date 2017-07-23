.PHONY: view

vid.mp4: frame0.png Makefile
	ffmpeg -y -r 24 -f image2 -i frame%d.png -vcodec libx264 -crf 25 -pix_fmt yuv420p vid.mp4

frame0.png: src/Example.hs app/Main.hs Makefile
	rm -f frame*.png
	stack build
	stack exec haskell-animation-exe

frameT${TIME}.png: src/Example.hs app/Main.hs Makefile
	rm -f frameT${TIME}.png
	stack build
	stack exec haskell-animation-exe "${TIME}"

view: frameT${TIME}.png
	feh frameT${TIME}.png
