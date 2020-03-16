let poirotsrc = Logs.Src.create "poirot" ~doc:"Poirot events";;
module L = (val Logs.src_log poirotsrc : Logs.LOG)
