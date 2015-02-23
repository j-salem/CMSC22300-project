all:
	cd src/elm/ && elm-make Game/Main.elm --output=main.html && mv main.html ../../

clean:
	rm -rf elm-* main.html &&  cd src/elm/ && rm -rf elm-* main.html


