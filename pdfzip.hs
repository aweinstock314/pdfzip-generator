{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Codec.Archive.Zip
import Pdf.Toolbox.Document
import qualified Data.ByteString.Lazy as L

generateSampleZip = do
    let helloDotCee = "#include <stdio.h>\nint main() { return printf(\"Hello, world!\\n\"); }\n"
    let sampleZipArchive = addEntryToArchive (toEntry "hello/hello.c" 0 helloDotCee) emptyArchive
    withBinaryFile "hello.zip" WriteMode $ \h -> L.hPutStr h $ fromArchive sampleZipArchive

main = generateSampleZip
