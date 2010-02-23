
s-protobuf/index.html: manual.xml
	mkdir -p s-protobuf
	xmlto -o s-protobuf xhtml $< 
	@echo "Done"

manual.xml: manual.lisp
	sbcl --noinform --load $< --eval "(expand)" --eval "(quit)"

ul: s-protobuf/index.html
	scp -r s-protobuf acme:public_html/docs

clean:
	rm -rf *.html *.xml *.fasl s-protobuf
