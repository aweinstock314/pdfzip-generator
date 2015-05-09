{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
import Codec.Archive.Zip
import Control.Monad
import Pdf.Toolbox.Core
import Pdf.Toolbox.Document
import Pdf.Toolbox.Document.Internal.Types
import System.IO
import qualified Data.ByteString.Lazy as L
import qualified System.IO.Streams as Streams

lprint = liftIO . print

sampleZipArchive = addEntryToArchive (toEntry "hello/hello.c" 0 helloDotCee) emptyArchive where
    helloDotCee = "#include <stdio.h>\nint main() { return printf(\"Hello, world!\\n\"); }\n"

generateSampleZip = do
    withBinaryFile "hello.zip" WriteMode $ \h -> L.hPutStr h $ fromArchive sampleZipArchive

copyPdfFile srcFname dstFname = do
    withBinaryFile srcFname ReadMode $ \input -> do
        runPdfWithHandle input knownFilters $ do
            Document stream (Dict table) <- document
            lprint stream
            lprint table
            liftIO . Streams.withFileAsOutput dstFname $ \output -> do
                runPdfWriter output $ do
                    writePdfHeader
                    forM_ table $ \(name, obj) -> lprint (name, obj)
        return () -- It seems like runPdfWithHandle automatically prints "Right ()" unless suppressed

main = copyPdfFile "sample.pdf" "sample.copy.pdf"
