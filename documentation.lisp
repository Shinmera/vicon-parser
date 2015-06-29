(in-package #:vicon-parser)

(defmacro setdocs (&body pairs)
  `(progn
     ,@(loop for (var doc) in pairs
             collect (destructuring-bind (var &optional (type 'function))
                         (if (listp var) var (list var))
                       `(setf (documentation ',var ',type) ,doc)))))

(setdocs
  ((with-retry-restart)
   "Wrapper that establishes a restart which retries evaluating BODY on invocation.")

  (translate-name
   "Translate a vicon name into a more lispy one by replacing #\: with #\-, #\_ with #\- and turning the result into a KEYWORD.")

  (parse-timestamp
   "Parse a vicon file timestamp. The timestamp has to be in the format of YYYYMMDD_HHMMSS.
Returns a UNIVERSAL-TIME.")

  ((vicon-file type)
   "Container for data of a vicon data file.")

  (file
   "Returns the file that was parsed.")

  (timestamp
   "Returns the timestamp that marks the approximate beginning of the recording.")

  (description
   "Returns the description of the vicon file that was parsed.")

  (resolution
   "Returns the timing resolution in frames per second.")

  (markers
   "Returns an alist consisting of (MARKERNAME FIELDLIST METRICLIST).")

  (frames
   "Returns a vector of FRAMEs.")

  ((frame type)
   "Container for a vicon data frame at a specific time.")

  (vicon-file
   "Returns the file this frame is a part of.")

  (id
   "Returns the frame's ID.")

  (sub-id
   "Returns the frame's Sub-ID, if it was set.")

  (points
   "Returns an EQL hash-table of MARKERNAME to MARKER-POINT.")

  (marker-point
   "Returns a the MARKER-POINT named by NAME from FRAME.")

  ((marker-point type)
   "Container for a single marker point on a specific frame.")

  (marker
   "Returns the name of the marker.")

  (data
   "Returns an EQL hash-table of FIELDNAME -> DATA. DATA is usually a FLOAT.")

  (metrics
   "Returns an EQL hash-table of FIELDNAME -> METRIC. METRIC is usually a KEYWORD.")

  (field
   "Returns the DATA for FIELD stored in MARKER-POINT.")

  (field-metric
   "Returns the METRIC for FIELD stored in MARKER-POINT.")

  (parse-marker-list
   "Parses the three lists of markers, fields, and metrics into an alist of the form (FIELDNAME MARKERS METRICS).")

  (parse-field-for-metric
   "Attempts to appropriately parse FIELD for the given METRIC.")

  (parse-frame
   "Parse the given DATA for VICON-FILE and return the fully parsed resulting FRAME.
DATA is expected to be a list representing all columns in a row of the vicon CSV file.")

  (read-clean-line
   "Read a line (if possible), but trim it of spaces, newlines, and returns.")

  (split
   "Split STRING by CHAR. Includes empty subsequences in the resulting list.")

  (read-vicon-header
   "Read the header parts of a vicon CSV file. Returns a prepared VICON-FILE instance.")

  (map-read-vicon-frames
   "Read a from STREAM and call FUNCTION with each FRAME read.
The frame is /not/ automatically pushed onto the FRAMES of the VICON-FILE.")

  (parse-vicon-file
   "Completely parse PATHNAME to a VICON-FILE including all frames."))
