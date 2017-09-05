\begin{code}
module MIPSInterpreter (
interpretMIPSWithSPIM) where
import Data.Text.Prettyprint.Doc
import System.IO(hPutStr, hFlush, Handle, FilePath)
import System.IO.Temp(withSystemTempFile)
import System.Process(readProcessWithExitCode)
import System.Exit(ExitCode(..))
import MIPSAsm
import Text.Read(readMaybe)
import PrettyUtils
import Safe(lastMay)
type ErrorDoc = Doc ()

-- | Allow for interpreters that try to access state.
interpretMIPSWithSPIM :: MProgram -> IO (Either ErrorDoc Int)
interpretMIPSWithSPIM p = 
    withSystemTempFile "mipsfile" (\filepath handle -> do
        _writeMIPSIntoFile p handle
        _runMIPSFromFileWithSPIM filepath)


-- | Write MIPS code into the file owned by Handle
_writeMIPSIntoFile :: MProgram -> Handle -> IO ()
_writeMIPSIntoFile program handle = do
    hPutStr handle (docToString . printMIPSAsm $ program)
    hFlush handle


-- | Run MIPS code through SPIM with the file.
_runMIPSFromFileWithSPIM :: FilePath -> IO (Either ErrorDoc Int)
_runMIPSFromFileWithSPIM path = do
    let stdin = ""

    (exitcode, stdout, stderr) <- readProcessWithExitCode "spim" ["-f", path] stdin
    case exitcode of 
        ExitFailure i ->
            return $ Left $ 
                vcat [pretty "exited with failure code: " <+> pretty i,
                        pretty "stdout:",
                        pretty stdout,
                        pretty "stderr: ",
                        pretty stderr]
        ExitSuccess ->
            case lastMay (lines stdout) >>= readMaybe of
                Just val -> return $ Right val
                Nothing -> return $ Left $
                                vcat [pretty "program returned non-integer output:",
                                      pretty "stderr:",
                                      pretty stderr,
                                      pretty "stdout:",
                                      pretty stdout]
\end{code}
