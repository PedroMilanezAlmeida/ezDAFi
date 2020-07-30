package com.flowjo.plugin.ezDAFi.dictionary;

public class VDJ {

    public enum Manufacturer {
        BD, TENX {
            @Override
            public String toString() {
                return "10X";
            }
        }, IGMT
    }

    public enum ChainType {
        BCR_HEAVY {
            @Override
            public String toString() {
                return "Heavy";
            }
        }, BCR_KAPPA {
            @Override
            public String toString() {
                return "Kappa";
            }
        }, BCR_LAMBDA {
            @Override
            public String toString() {
                return "Lambda";
            }
        }, TCR_ALPHA {
            @Override
            public String toString() {
                return "Alpha";
            }
        }, TCR_BETA {
            @Override
            public String toString() {
                return "Beta";
            }
        }, TCR_DELTA {
            @Override
            public String toString() {
                return "Delta";
            }
        }, TCR_GAMMA {
            @Override
            public String toString() {
                return "Gamma";
            }
        },
        NONE,
        MULTI,
    }

    public enum Chain {
        TRA, TRB, LAMBDA, KAPPA, HEAVY, NONE, MULTI,
    }

    public enum Type {
        GAMMA_DELTA{
            @Override
            public String toString() {
                return "TCR";
            }
        }, ALPHA_BETA{
            @Override
            public String toString() {
                return "TCR";
            }
        }, BCR,
    }

    public enum CellType {
        B,
        DENDRITIC {
            @Override
            public String toString() {
                return "Dendritic";
            }
        },
        MONOCYTE_CLASSICAL {
            @Override
            public String toString() {
                return "Monocyte_classical";
            }
        },
        MONOCYTE_NONCLASSICAL {
            @Override
            public String toString() {
                return "Monocyte_nonclassical";
            }
        },
        NATURAL_KILLER {
            @Override
            public String toString() {
                return "Natural_killer";
            }
        },
        T_CD4_MEMORY {
            @Override
            public String toString() {
                return "T_CD4_memory";
            }
        },
        T_CD4_NAIVE {
            @Override
            public String toString() {
                return "T_CD4_naive";
            }
        },
        T_CD8_MEMORY {
            @Override
            public String toString() {
                return "T_CD8_memory";
            }
        },
        T_CD8_NAIVE {
            @Override
            public String toString() {
                return "T_CD8_naive";
            }
        },
        T_GAMMA_DELTA {
            @Override
            public String toString() {
                return "T_gamma_delta";
            }
        },
    }
}
