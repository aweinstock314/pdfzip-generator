{-# LANGUAGE LambdaCase, NoMonomorphismRestriction, OverloadedStrings #-}
import Codec.Archive.Zip
import Control.Monad
import Control.Monad.Trans
import Data.Int
import Debug.Trace
import Pdf.Toolbox.Core
import Pdf.Toolbox.Document
import Pdf.Toolbox.Document.Internal.Types
import System.IO
import qualified Data.ByteString.Lazy as L
import qualified Data.HashSet as HS
import qualified System.IO.Streams as Streams

lprint = liftIO . print

sampleZipArchive = addEntryToArchive (toEntry "hello/hello.c" 0 helloDotCee) emptyArchive where
    helloDotCee = "#include <stdio.h>\nint main() { return printf(\"Hello, world!\\n\"); }\n"

generateSampleZip = do
    withBinaryFile "hello.zip" WriteMode $ \h -> L.hPutStr h $ fromArchive sampleZipArchive

copyPdfFile srcFname dstFname = do
    withBinaryFile srcFname ReadMode $ \input -> do
        Streams.withFileAsOutput dstFname $ \output -> do
            runPdfWithHandle input knownFilters $ do
                Document stream table <- document
                --lprint stream
                --lprint table
                runPdfWriter output $ do
                    writePdfHeader
                    --forM_ table $ \(name, obj) -> lprint (name, obj)
                    emitCyclic HS.empty (ODict table)
                    writeXRefTable 0 (deleteValueForKey "Prev" table)
        return () -- It seems like runPdfWithHandle automatically prints "Right ()" unless suppressed

-- Adapted from example at https://github.com/Yuras/pdf-toolbox/blob/master/examples/unpack-and-decrypt-pdf-file.hs
emitCyclic :: HS.HashSet Int -> Object Int64 -> PdfWriter (Pdf IO) ()
emitCyclic seenSoFar = \case
    ODict (Dict vals) -> forM_ vals $ emitCyclic seenSoFar . mapObject (error "impossible") . snd
    OArray (Array vals) -> forM_ vals $ emitCyclic seenSoFar . mapObject (error "impossible")
    ORef r@(Ref index _) -> do
        unless (index `HS.member` seenSoFar) $ do
            lift (lookupObject r) >>= \case
                o@(OStream s) -> do
                    (lift $ loadRawStream s) >>= writeObject r
                    emitCyclic (HS.insert index seenSoFar) o
                x -> trace (show x) (return ())
    _ -> return ()

loadRawStream :: Stream Int64 -> Pdf IO (Object L.ByteString)
loadRawStream s@(Stream d _) = do
    l <- lookupDict "Length" d >>= deref >>= fromObject >>= intValue
    ris <- getRIS
    Stream _ is <- rawStreamContent ris l s
    content <- liftIO $ L.fromChunks `liftM` Streams.toList is
    return $ OStream $ Stream d content

main = copyPdfFile "sample.pdf" "sample.copy.pdf"
