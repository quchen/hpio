EXENAME = Main

all :
	ghc -O --make -i:. -o $(EXENAME) Main.hs

clean :
	rm -f $(EXENAME)
	find -iname "*.prof" -delete
	find -iname "*.eventlog" -delete
	find -iname "*.hi" -delete
	find -iname "*.o" -delete

doc :
	haddock -h Main.hs -o doc
