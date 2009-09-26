
s-protobuf/index.html: manual.xml
	mkdir -p s-protobuf
	xmlto -o s-protobuf xhtml $< 
	@echo "Done"

manual.xml: manual.lisp
	sbcl --noinform --load $< --eval "(expand)" --eval "(quit)"

clean:
	rm -rf *.html *.xml *.fasl s-protobuf
