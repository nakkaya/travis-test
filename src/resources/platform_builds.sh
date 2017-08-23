wget http://ferret-lang.org/build-artifacts/packr.jar
wget http://ferret-lang.org/build-artifacts/openjdk-1.7.0-u80-unofficial-macosx-x86_64-image.zip
wget http://ferret-lang.org/build-artifacts/openjdk-1.7.0-u80-unofficial-linux-amd64-image.zip
wget http://ferret-lang.org/build-artifacts/openjdk-1.7.0-u80-unofficial-windows-amd64-image.zip

java -jar packr.jar \
     --platform windows64 \
     --jdk openjdk-1.7.0-u80-unofficial-windows-amd64-image.zip \
     --executable ferret \
     --classpath target/ferret.jar \
     --mainclass ferret.core \
     --minimizejre soft \
     --output ferret

mv ferret/jre/bin/msvcr100.dll ferret/
zip -r ferret-windows-amd64.zip ferret/
rm -r ferret/

java -jar packr.jar \
     --platform mac \
     --jdk openjdk-1.7.0-u80-unofficial-macosx-x86_64-image.zip \
     --executable ferret \
     --classpath target/ferret.jar \
     --mainclass ferret.core \
     --minimizejre soft \
     --output ferret

echo "DIR=\"\$( cd \"\$( dirname \"\${BASH_SOURCE[0]}\" )\" && pwd )\"" > ferret/ferret
echo "\$DIR/Contents/MacOS/ferret" >> ferret/ferret
chmod +x ferret/ferret
zip -r ferret-macosx-x86_64.zip ferret/
rm -r ferret/

java -jar packr.jar \
     --platform linux64 \
     --jdk openjdk-1.7.0-u80-unofficial-linux-amd64-image.zip \
     --executable ferret \
     --classpath target/ferret.jar \
     --mainclass ferret.core \
     --minimizejre soft \
     --output ferret

zip -r ferret-linux-amd64.zip ferret/
rm -r ferret/

rm packr.jar openjdk-1.7.0-u80-unofficial-macosx-x86_64-image.zip openjdk-1.7.0-u80-unofficial-linux-amd64-image.zip openjdk-1.7.0-u80-unofficial-windows-amd64-image.zip
