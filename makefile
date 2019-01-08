refresh:
	git fetch upstream
	git rebase upstream/master

build:
	fsharpc --nologo -a chess.fs
	fsharpc --nologo -r chess.dll -a pieces.fs
	fsharpc --nologo -r chess.dll -r pieces.dll chessApp.fsx

	
