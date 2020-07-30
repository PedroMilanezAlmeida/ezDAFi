package com.flowjo.plugin.DAFi.dictionary;

public class VDJML {

    public static class SYSTEM_ATTRIBUTES {
        public static final String DESKTOP = "Desktop";
        public static final String USER_HOME = "user.home";
    }

    public static class VDJ_10X {
        public static final String BARCODE = "barcode";
        public static final String IS_CELL = "is_cell";
        public static final String CONTIG = "contig_id";
        public static final String IS_HIGH_CONFIDENCE = "high_confidence";
        public static final String LENGTH = "length";
        public static final String CHAIN = "chain";
        public static final String VARIABLE_GENE = "v_gene";
        public static final String DIVERSITY_GENE = "d_gene";
        public static final String JOINING_GENE = "j_gene";
        public static final String CONSTANT_GENE = "c_gene";
        public static final String FULL_LENGTH = "full_length";
        public static final String IS_PRODUCTIVE = "productive";
        public static final String CDR3R = "cdr3";
        public static final String CDR3R_NT = "cdr3_nt";
        public static final String READS = "reads";
        public static final String UMIS = "umis";
        public static final String RAW_CLONOTYPE_ID = "raw_clonotype_id";
        public static final String RAW_CONCENSUS_ID = "raw_consensus_id";
        public static final String VDJ_GATED = "VDJ_Gated";
        public static final String NONE = "None";
    }

    // IMGT_VQuest
    public static class VDJ_IMGT_VQUEST {
        public static final String VENDOR_NAME = "IMGT/VQuest";
        public static final String BARCODE = "Sequence ID"; // v
        //        public static final String IS_CELL = "is_cell";
//        public static final String CONTIG = "contig_id";
//        public static final String IS_HIGH_CONFIDENCE = "high_confidence";
        public static final String LENGTH = "CDR3-IMGT length"; //
        //        public static final String CHAIN = "chain";
        public static final String VARIABLE_GENE = "V-GENE and allele"; //
        public static final String DIVERSITY_GENE = "D-GENE and allele"; //
        public static final String JOINING_GENE = "J-GENE and allele"; //
        //        public static final String CONSTANT_GENE = "c_gene";
        public static final String FULL_LENGTH = "JUNCTION frame"; // Might not be the same? -MVP
        public static final String IS_PRODUCTIVE = "V-DOMAIN Functionality"; // v
        //        public static final String CDR3R = "cdr3";
        public static final String CDR3R_NT = "AA JUNCTION"; //
//        public static final String READS = "reads";
//        public static final String UMIS = "umis";
//        public static final String RAW_CLONOTYPE_ID = "raw_clonotype_id";
//        public static final String RAW_CONCENSUS_ID = "raw_consensus_id";
//        public static final String VDJ_GATED = "VDJ_Gated";
//        public static final String NONE = "None";

        public static final String IN_FRAME = "in-frame";
    }

    public static class VDJ_BD_Rhapsody {
        public static final String VENDOR_NAME = "BD";
        public static final String BARCODE = "Cell_Index";
        public static final String IS_PRODUCTIVE = "V-DOMAIN Functionality"; // v
        public static final String CDR3R_NT = "AA JUNCTION"; //

        public static class TCRA {
            public static final String VARIABLE_GENE = "TCRa V"; //
            public static final String DIVERSITY_GENE = "TCRa D"; //
            public static final String JOINING_GENE = "TCRa J"; //
            public static final String CONSTANT_GENE = "TCRa C";
            public static final String CDR3R = "TCRa AA CDR3";
            public static final String READS = "TCRa DBEC Corrected Reads Dominant";
            public static final String COUNTS = "TCRa DBEC Corrected molecule count for Dominant CDR3";
            public static final String PERCENT_DOMINANT = "TCRa % DBEC Reads Dominant";
        }

        public static class TCRB {
            public static final String VARIABLE_GENE = "TCRb V"; //
            public static final String DIVERSITY_GENE = "TCRb D"; //
            public static final String JOINING_GENE = "TCRb J"; //
            public static final String CONSTANT_GENE = "TCRb C";
            public static final String CDR3R = "TCRb AA CDR3";
            public static final String READS = "TCRb DBEC Corrected Reads Dominant";
            public static final String COUNTS = "TCRb DBEC Corrected molecule count for Dominant CDR3";
            public static final String PERCENT_DOMINANT = "TCRb % DBEC Reads Dominant";
        }
    }

    public static class VDJ_VERBAGE {
        public static final String SKIP = "Skip";
        public static final String UNIQUE = "Unique";
        public static final String PRODUCTIVE = "Productive";
        public static final String FULL_LENGTH = "Full Length";
        public static final String CONTIG_ID = "Contig ID";
        public static final String CDR3_NT = "CDR3 NT";
        public static final String BARCODE = "Barcode";
        public static final String BARCODES = "Barcodes";
        public static final String SEQUENCE = "Sequence";
        public static final String CLONOTYPE = "Clonotype";
        public static final String REPERTOIRE_SUMMARY = "Repertoire Summary";
        public static final String VARIABLE_GENE_USAGE = "Variable Gene Usage";
        public static final String DIVERSITY_GENE_USAGE = "Diversity Gene Usage";
        public static final String JOINING_GENE_USAGE = "Joining Gene Usage";
        public static final String CONSTANT_GENE_USAGE = "Constant Gene Usage";
        public static final String CLONOTYPES_DIVERSITY = "Clonotypes Diversity";
        public static final String CDR3 = "Complementary Determining Region 3";
        public static final String CDR3_SHORT = "CDR3";
        public static final String CDR3_SEQUENCE = "Complementary Determining Region 3 Sequence";
        public static final String CDR3_LENGTH = "Complementary Determining Region 3 Length";
        public static final String POPULATION = "Population";
        public static final String CHAIN = "Chain";
        public static final String VARIABLE = "Variable";
        public static final String DIVERSITY = "Diversity";
        public static final String JOINING = "Joining";
        public static final String CELLID = "CellId";
        public static final String VDJ_PREFIX = "VDJ_";
        public static final String NOT_ASSIGNED = "Not_Assigned";
        public static final String NA = "NA";
        //TCR 
        public static final String TRA = "TRA";
        public static final String TCR = "TCR";
        public static final String TRB = "TRB";
        // BCR
        public static final String BCR = "BCR";
        public static final String IGK = "IGK";
        public static final String IGL = "IGL";
        public static final String IGH = "IGH";

        public static final String CONSTANT = "Constant";
        public static final String NONE = "NONE";
        public static final String EMPTY = "";
        public static final String COUNT = "Count";
        public static final String CONCENSUS = "Concensus";

    }

    public static class VDJ_Cell {
        public static final String BARCODEID = "barcodeID";
        public static final String ISCELL = "isCell";
        public static final String CONTIGID = "";
        public static final String IS_HIGH_CONFIDENCE = "isHighConfidence";
        public static final String LENGTH = "length";
        public static final String CHAIN_ID = "chainID";
        public static final String VARIABLE_GENE = "variableGene";
        public static final String DIVERSITY_GENE = "diversityGene";
        public static final String JOINING_GENE = "joiningGene";
        public static final String CONSTANT_GENE = "CONSTANT_GENE";
        public static final String IS_FULL_LENGTH = "isFullLength";
        public static final String IS_PRODUCTIVE = "isProductive";
        public static final String IDENTIFIER_CDR3 = "identifierCDR3";
        public static final String SEQUENCE_CDR3 = "sequenceCDR3";
        public static final String COUNT_READS = "countReads";
        public static final String COUNT_UMI = "countUMI";
        public static final String RAW_CLONOTYPE_ID = "rawClonotypeID";
        public static final String RAW_CONCENSUS_ID = "rawConcensusID";
        public static final String IS_VDJ_GATED = "isVDJ_Gated";
        public static final String OWNING_SAMPLE = "owningSample";
        public static final String OWNING_POPULATION = "owningPopulation";
        public static final String METADTA_CSV = "metadataCSV";
        public static final String METADATA_CSV_SHORT = "metadataCSVShort";
        public static final String OWNED = "owned";
        public static final String REMOVED = "removed";
        public static final String CSV_TOTAL_ROWS = "CSV_TOTAL_ROWS";
    }

    public static class DIALOG_VERBAGE {

        public static final String CHANGE_TITLE = "Change Title";
        public static final String TOGGLE_EDIT_CHART = "Toggle Edit Chart";
        public static final String SUCCESS = "Success";
        public static final String SAVE = "Save";
        public static final String CANCEL = "Cancel";
        public static final String TO_LAYOUT = "To Layout";
        public static final String TO_FILE = "To FILEML";
        public static final String SAVE_VDJ_GRAPHS = "Save VDJ_Explorer Graph";
        public static final String SAVE_VDJ_GRAPH = "Save VDJ_Explorer Graph";
        public static final String ZERO = "0";
        public static final String ERROR = "Error";
        public static final String INFORMATION = "Information";
        public static final String WARNING = "Warning";
        public static final String SAVE_CHARTS_TO_LAYOUT = "Save Chart to FILEML";
        public static final String SAVE_CHARTS_TO_FILE = "Save Charts to FILEML";
        public static final String SAVE_CHART_TO_FILE = "Save Chart to FILEML";
        public static final String SAVE_CHART_TO_LAYOUT = "Save Chart to Layout";
        public static final String EXPORT_CHART_DATA = "Export Chart Data";

    }

    public static class MOUSE_ACTIONS_CSS {
        public static final String ON_SELECTED_CLASS = "onSelected";
        public static final String ON_HOVER_CLASS = "onSelected";
        public static final String ON_SELECTED_ADD_STYLE = "-fx-background-color: #00bfa5;";
        public static final String ON_SELECTED_REMOVE_STYLE = "-fx-background-color: grey;";
        public static final String ON_HOVER_ADD_STYLE = "-fx-background-color: #ff5722;";
        public static final String ON_HOVER_REMOVE_STYLE = "-fx-background-color: grey";

    }

    public static class FLOWJO_SITES {
        public static final String FLOWJO_COM = "http://www.seqgeq.com";

    }

    public static class FLOWJO_EMAILS {
        public static final String TECH_SUPPORT = "techsupport@seqgeq.com";

    }

    public static class VDJ_CHART_VERBAGE {
        public static final String SDEI = "Shannon Divergence Entropy Index";
        public static final String IRAD = "IRad";
        public static final String JOINING_IRAD_MATRIX = "Joining " + IRAD + " Matrix";
        public static final String VARIABLE_IRAD_MATRIX = "Variable " + IRAD + " Matrix";
        public static final String CLONOTYPE_DIVERSITY = "Clonotype Diversity";
        public static final String CLONOTYPE_DIVERSITY_MATRIX = "Clonotype Diversity Matrix";
        public static final String JOINING_JSD = "Joining " + IRAD;
        public static final String JSD = "JSD";
        public static final String VARIABLE_JSD = "Variable " + IRAD;

    }
}
