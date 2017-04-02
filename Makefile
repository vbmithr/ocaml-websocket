all:
	jbuilder build @install test/reynir.exe test/upgrade_connection.exe

clean:
	rm -rf _build
