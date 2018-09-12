all:
	dune build @install test/reynir.exe test/upgrade_connection.exe

clean:
	dune clean
