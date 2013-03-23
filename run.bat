mkdir bin

scalac -optimise -d bin -cp javacv.jar;javacpp.jar;javacv-windows-x86_64.jar CamKey.scala
scala -cp bin;javacv.jar;javacpp.jar;javacv-windows-x86_64.jar CamKey
