package com.flowjo.plugin.DAFi.utilities;

public class FJSML {

    public static class FORMATS {
        public static class FILE {

            public static final class FCS {
                public static final String REGEX_EXTENSION = "*.fcs";
                public static final String UPPERCASE = "FCS";
                public static final String LOWERCASE = "fcs";
                public static final String CAMELCASE = "Fcs";
                public static final String EXTENSION = ".fcs";
            }

            public static final class H5 {
                public static final String REGEX_EXTENSION = "*.h5";
                public static final String UPPERCASE = "H5";
                public static final String LOWERCASE = "h5";
                public static final String CAMELCASE = "H5";
                public static final String EXTENSION = ".h5";
            }

            public static final class CSV {
                public static final String REGEX_EXTENSION = "*.csv";
                public static final String UPPERCASE = "CSV";
                public static final String LOWERCASE = "csv";
                public static final String CAMELCASE = "Csv";
                public static final String EXTENSION = ".csv";
            }

            public static final class TSV {
                public static final String REGEX_EXTENSION = "*.tsv";
                public static final String UPPERCASE = "TSV";
                public static final String LOWERCASE = "tsv";
                public static final String CAMELCASE = "Tsv";
                public static final String EXTENSION = ".tsv";
            }

            public static final class TXT {
                public static final String REGEX_EXTENSION = "*.txt";
                public static final String UPPERCASE = "TXT";
                public static final String LOWERCASE = "txt";
                public static final String CAMELCASE = "Txt";
                public static final String EXTENSION = ".txt";
            }
        }

        public static class IMAGE {

            public static final class JPEG {
                public static final String REGEX_EXTENSION = "*.jpeg";
                public static final String UPPERCASE = "JPEG";
                public static final String LOWERCASE = "jpeg";
                public static final String EXTENSION = ".jpeg";
            }

            public static final class PNG {
                public static final String LOWERCASE = "png";
                public static final String REGEX_EXTENSION = "*.png";
                public static final String UPPERCASE = "PNG";
                public static final String EXTENSION = ".png";
            }
        }
    }

    public static class CHARS {
        public static final char UNDERSCORE = '_';
        public static final char DASH = '-';
    }

    public static class SYSTEM_ATTRIBUTES {
        public static final String DESKTOP = "Desktop";
        public static final String USER_HOME = "user.home";
    }

    public static class COMMON {
        public static final String CELL_ID = "CellId";
        public static final String SAMPLE = "Sample";
        public static final String DESKTOP = "Desktop";
        public static final String ID = "ID";
        public static final String SAMPLE_ID = "SampleID";
        public static final String SEQGEQ = "seqgeq";
        public static final String FLOWJO = "seqgeq";
    }

    public static class SEQGEQ {
        public static final String GENESET = "Geneset";
        public static final String GENESET_BOXED = "["+GENESET+"]";
        public static final String GENE_SET_NAME = "$GeneSetName";
        public static final String GENE_SET_NAME_DOLLAR_SIGN = "$"+GENE_SET_NAME;
        public static final String GENE = "Gene";
        public static final String GENE_BOXED = "["+GENE+"]";
    }

    public static class BD {
        public static final String CELL_INDEX = "Cell_Index";
    }


    public static class HTTP_CODES {
        public static class INFORMATIONAL {
            public static int CONTINUE = 100;
            public static int SWITCHING_PROTOCOLS = 101;
            public static int PROCESSING = 102;
        }

        public static class SUCCESS {
            /**
             * General success status code. This is the most common code. Used to indicate success.
             */
            public static int OK = 200;
            public static int CREATED = 201;
            public static int ACCEPTED = 202;
            public static int NON_AUTHORITATIVE_INFORMATION = 203;
            public static int NO_CONTENT = 204;
            public static int RESET_CONTENT = 205;
            public static int PARTIAL_CONTENT = 206;
            public static int MULTI_STATUS = 207;
            public static int ALREADY_REPORTED = 208;
            public static int IM_USED = 209;
        }

        public static class REDIRECTION {
            public static int MULTIPLE_CHOICES = 300;
            public static int MOVED_PERMANENTLY = 301;
            public static int FOUND = 302;
            public static int SEE_OTHER = 303;
            public static int NOT_MODIFIED = 304;
            public static int USE_PROXY = 305;
            public static int NA = 306;
            public static int TEMPORARY_REDIRECT = 307;
            public static int PERMANENT_REDIRECT = 308;
        }

        public static class CLIENT_ERROR {
            public static int BAD_REQUEST = 400;
            public static int UNAUTHORIZED = 401;
            public static int PAYMENT_REQUIRED = 402;
            public static int FORBIDDEN = 403;
            public static int NOT_FOUND = 404;
            public static int METHOD_NOT_ALLOWED = 405;
            public static int NOT_ACCEPTABLE = 406;
            public static int PROXY_AUTHENTICATED_REQUIRED = 407;
            public static int REQUEST_TIMEOUT = 408;
            public static int CONFLICT = 409;
            public static int GONE = 410;
            public static int LENGTH_REQUIRED = 411;
            public static int PRECONDITION_FAILED = 412;
            public static int REQUEST_ENTITY_TOO_LARGE = 413;
            public static int REQUEST_URI_TOO_LONG = 414;
            public static int UNSUPPORTED_MEDIA_TYPE = 415;
            public static int REQUESTED_RANGE_NOT_SATISFIABLE = 416;
            public static int EXPECTATION_FAILED = 417;
            public static int IM_A_TEAPOT = 418;
            public static int ENHANCE_YOUR_CLAIM = 420;
            public static int UNPROCESSABLE_ENTITY = 422;
            public static int LOCKED = 423;
            public static int FAILED_DEPENDENCY = 424;
            public static int RESERVED_FOR_WEBDAV = 425;
            public static int UPGRADE_REQUIRED = 426;
            public static int PRECONDITION_REQUIERD = 428;
            public static int TOO_MANY_REQUESTS = 429;
            public static int REQUEST_HEADER_FIELDS_TOO_LARGE = 431;
            public static int NO_RESPONSE = 444;
            public static int RETRY_WITH = 449;
            public static int BLOCKED_BY_WINDOWS_PARENTAL_CONTROLS = 450;
            public static int UNAVAIlABLE_FOR_LEGAL_REASONS = 451;
            public static int CLIENT_CLOSED_REQUEST = 499;
        }

        public static class SERVER_ERROR {
            public static int INTERNAL_SERVER_ERROR = 500;
            public static int NOT_IMPLEMENTED = 501;
            public static int BAD_GATEWAY = 502;
            public static int SERVICE_UNAVAILABLE = 503;
            public static int GATEWAY_TIMEOUT = 504;
            public static int HTTP_VERSION_NOT_SUPPORTED = 505;
            public static int VARIANT_ALSO_NEGOTIATES = 506;
            public static int INSUFFICIENT_STORAGE = 507;
            public static int LOOP_DETECTED = 508;
            public static int BANDWIDTH_LIMIT_EXCEEDED = 509;
            public static int NOT_EXTENDED = 510;
            public static int NETWORK_AUTHENTICATION_REQUIRED = 511;
            public static int NETWORK_READ_TIMEOUT_ERROR = 598;
            public static int NETWORK_CONNECT_TIMEOUT_ERROR = 599;
        }
    }
}
