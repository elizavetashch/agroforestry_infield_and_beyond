# =============================================================================
# Hypothesis Network Visualisation
# Based on Grames (2022) — built directly with igraph + ggraph
# No dependency on hypoweavr (package is too early-stage to rely on)
# =============================================================================

# --- 1. Install & load packages ----------------------------------------------

pkgs <- c("igraph", "ggraph", "tidygraph", "dplyr", "ggplot2", "ggrepel")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

library(igraph)
library(ggraph)
library(tidygraph)
library(dplyr)
library(ggplot2)

# --- 2. Load data ------------------------------------------------------------

# Make sure hypoweavr_ready_v2.csv is in your working directory
# df <- read.csv("hypoweavr_ready_v2.csv", stringsAsFactors = FALSE)

# --- 3. Parse pathways into edges --------------------------------------------
# Each pathway is "A > B" or "A > B > C"
# Split into individual directed edges, one row per edge

parse_pathway <- function(pathway, direction, source) {
  nodes <- trimws(strsplit(pathway, ">")[[1]])
  if (length(nodes) < 2) return(NULL)
  edges <- data.frame(
    from      = nodes[-length(nodes)],
    to        = nodes[-1],
    direction = direction,
    source    = source,
    stringsAsFactors = FALSE
  )
  return(edges)
}

edge_list <- do.call(rbind, mapply(
  parse_pathway,
  df$pathway,
  df$direction,
  df$source,
  SIMPLIFY = FALSE
))

# Summarise: where same edge appears in multiple sources, keep most common
# direction and count sources
edge_summary <- edge_list %>%
  group_by(from, to) %>%
  summarise(
    direction = {
      tab <- table(direction)
      if (length(tab) > 1) "mixed" else names(tab)[which.max(tab)]
    },
    n_sources = n(),
    .groups = "drop"
  )

# --- 4. Build igraph object --------------------------------------------------

all_nodes <- unique(c(edge_summary$from, edge_summary$to))

# Assign node type
predictor_nodes <- c("in-field complexity", "farming system")
moderator_nodes <- c("phenology", "arthropod trophic group", "habitat type",
                     "organic farming", "conventional farming",
                     "crop type", "tree species", "management intensity",
                     "landscape heterogeneity", "vegetation management",
                     "tree age", "disturbance regime")
response_nodes  <- c("arthropod diversity", "biodiversity", "crop yield",
                     "natural enemy abundance", "pest abundance",
                     "pest control", "regulating ecosystem services",
                     "in-field complexity effectiveness")

node_df <- data.frame(
  name = all_nodes,
  node_type = case_when(
    all_nodes %in% predictor_nodes ~ "Predictor",
    all_nodes %in% moderator_nodes ~ "Moderator",
    all_nodes %in% response_nodes  ~ "Response",
    TRUE                            ~ "Other"
  ),
  stringsAsFactors = FALSE
)

g <- tbl_graph(
  nodes    = node_df,
  edges    = edge_summary,
  directed = TRUE
)

# --- 5. Colour palettes ------------------------------------------------------

direction_colours <- c(
  "positive"   = "#2E8B57",
  "negative"   = "#C0392B",
  "mixed"      = "#E67E22",
  "moderation" = "#8E44AD"
)

node_colours <- c(
  "Predictor" = "#1A5276",
  "Moderator" = "#7D6608",
  "Response"  = "#1E8449",
  "Other"     = "#717D7E"
)

# --- 6. Full network plot ----------------------------------------------------

set.seed(42)

p_full <- ggraph(g, layout = "stress") +
  geom_edge_link(
    aes(colour = direction, width = n_sources),
    arrow     = arrow(length = unit(3, "mm"), type = "closed"),
    end_cap   = circle(4, "mm"),
    start_cap = circle(4, "mm"),
    alpha     = 0.85
  ) +
  scale_edge_colour_manual(values = direction_colours, name = "Direction") +
  scale_edge_width_continuous(range = c(0.5, 2.5), name = "No. of sources") +
  geom_node_point(
    aes(fill = node_type),
    shape = 21, size = 8, colour = "white", stroke = 1.2
  ) +
  scale_fill_manual(values = node_colours, name = "Node type") +
  geom_node_label(
    aes(label = name),
    repel         = TRUE,
    size          = 3,
    fontface      = "bold",
    label.padding = unit(0.15, "lines"),
    label.size    = 0.2,
    fill          = "white",
    alpha         = 0.9
  ) +
  labs(
    title    = "Hypothesis network: In-field complexity & beyond-field heterogeneity",
    subtitle = "Systematic literature synthesis following Grames (2022)",
    caption  = "Edge colour = direction  |  Edge width = number of supporting sources"
  ) +
  theme_graph(base_family = "sans") +
  theme(
    plot.title      = element_text(size = 13, face = "bold"),
    plot.subtitle   = element_text(size = 10, colour = "grey40"),
    plot.caption    = element_text(size = 8,  colour = "grey50"),
    legend.position = "right"
  )

print(p_full)
ggsave("hypothesis_network_full.png", p_full,
       width = 14, height = 10, dpi = 300, bg = "white")
message("Saved: hypothesis_network_full.png")

# --- 7. Focal subgraph: your H1 & H2 ----------------------------------------

focal_nodes <- c("in-field complexity", "landscape heterogeneity",
                 "crop yield", "biodiversity",
                 "pest control", "natural enemy abundance", "pest abundance")

g_focal <- g %>%
  activate(nodes) %>%
  filter(name %in% focal_nodes) %>%
  activate(edges) %>%
  filter(
    .N()$name[from] %in% focal_nodes,
    .N()$name[to]   %in% focal_nodes
  )

p_focal <- ggraph(g_focal, layout = "stress") +
  geom_edge_link(
    aes(colour = direction, width = n_sources),
    arrow     = arrow(length = unit(3.5, "mm"), type = "closed"),
    end_cap   = circle(5, "mm"),
    start_cap = circle(5, "mm"),
    alpha     = 0.9
  ) +
  scale_edge_colour_manual(values = direction_colours, name = "Direction") +
  scale_edge_width_continuous(range = c(0.8, 3), name = "No. of sources") +
  geom_node_point(
    aes(fill = node_type),
    shape = 21, size = 11, colour = "white", stroke = 1.5
  ) +
  scale_fill_manual(values = node_colours, name = "Node type") +
  geom_node_label(
    aes(label = name),
    repel         = TRUE,
    size          = 3.5,
    fontface      = "bold",
    label.padding = unit(0.2, "lines"),
    fill          = "white",
    alpha         = 0.95
  ) +
  labs(
    title    = "Focal subgraph: H1 & H2",
    subtitle = "IFC × beyond-field heterogeneity → productivity pathway"
  ) +
  theme_graph(base_family = "sans") +
  theme(
    plot.title      = element_text(size = 13, face = "bold"),
    legend.position = "right"
  )

print(p_focal)
ggsave("hypothesis_network_focal.png", p_focal,
       width = 10, height = 7, dpi = 300, bg = "white")
message("Saved: hypothesis_network_focal.png")

# --- 8. Node centrality table ------------------------------------------------
# Which concepts appear most often across hypotheses?

degree_df <- data.frame(
  node   = V(as.igraph(g))$name,
  degree = degree(as.igraph(g), mode = "all")
) %>%
  arrange(desc(degree))

message("\nNode degree (most connected concepts):")
print(degree_df)