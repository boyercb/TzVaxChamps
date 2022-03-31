
rerun_design <- FALSE

# declare design function -------------------------------------------------

# design function for the Access Challenge evaluation in Tanzania
access_evaluation <- function(n_clus = 40,
                              n_hh_per_clus = 200,
                              icc_clus = 0.10,
                              # rho = 0,
                              mde = 1.25,
                              base_rate = 0.16) {
  
  population <- declare_model(
    clus = fabricatr::add_level(N = n_clus),
    hh = fabricatr::add_level(
      N = n_hh_per_clus,
      # u_Y = rnorm(N, mean = rho * u_hh, sd = sqrt(1 - rho ^ 2)),
      Y_Z_0 = fabricatr::draw_binary_icc(
        prob = plogis(log(base_rate / (1 - base_rate))),
        N = N,
        clusters = clus,
        ICC = icc_clus
      ),
      Y_Z_1 = fabricatr::draw_binary_icc(
        prob = plogis(log(base_rate / (1 - base_rate)) + log(mde)),
        N = N,
        clusters = clus,
        ICC = icc_clus
      )
    )
  ) 
  
  estimand <- declare_inquiry(ate = mean(Y_Z_1 - Y_Z_0))
  
  assignment <- declare_assignment(Z = randomizr::cluster_ra(clusters = clus), legacy = FALSE)
  
  reveal_Y <- declare_measurement(Y = fabricatr::reveal_outcomes(Y ~ Z))
  
  ols <-  declare_estimator(
    Y ~ Z,
    model = estimatr::lm_robust,
    clusters = clus,
    inquiry = estimand,
    label = "ols + clustered host SEs"
  )
  
  design <- 
    population +
    estimand +
    assignment +
    reveal_Y +
    ols
  
  return(design)
}


# simulation parameters ---------------------------------------------------

access_design_comparisons <- expand_design(
  designer = access_evaluation,
  expand = TRUE,
  n_clus = c(40, 60, 80),
  n_hh_per_clus = c(100, 200),
  icc_clus = c(0.02, 0.05, 0.07),
  mde = c(1.2, 1.4, 1.6),
  base_rate = c(0.18, 0.24, 0.30)
)


# run simulations ---------------------------------------------------------

if (rerun_design) {
  plan(multisession, workers = 12)
  access_sims <- diagnose_designs(
    access_design_comparisons,
    sims = 500,
    bootstrap_sims = 100
  )
  plan(sequential)
  
  write_rds(access_sims, '0_data/access_sims.rds')
} else {
  access_sims <- read_rds('0_data/access_sims.rds')
}

access_sims_plot <- access_sims$diagnosands_df

# create nicely labeled factor variables for plotting purposes
access_sims_plot <-
  access_sims_plot %>%
  mutate(
    mde = factor(
      x = mde, 
      labels = c("OR = 1.2", "OR = 1.4", "OR = 1.6")
    ),
    base_rate = factor(
      x = base_rate, 
      labels = paste0("% vaccinated control = ", c("18%", "24%", "30%"))
    ),
    n_hh_per_clus = factor(
      x = n_hh_per_clus, 
      labels = paste0("N per cluster = ", c(100, 200))
    ),
    icc_clus = factor(
      x = icc_clus, 
      labels = paste0("ICC = ", c(0.02, 0.05, 0.07))
    )
  )


# plot results ------------------------------------------------------------

specd <- function(x, k) trimws(format(round(x, k), nsmall=k))

png("3_figures/power_plots.png", width = 13, height = 8, units = "in", res = 500)
ggplot(
  access_sims_plot,
  aes(
    x = n_clus,
    y = power,
    color = mde,
    shape = mde
  )
) +
  facet_grid(base_rate ~ n_hh_per_clus + icc_clus) +
  geom_hline(
    yintercept = .8,
    linetype = 2,
    color = "black",
    size = .2
  ) +
  geom_point() +
  geom_line() + 
  geom_text(
    aes(
      label = specd(power, 2),
      y = case_when(
        n_clus == 40 ~ power - 0.05,
        n_clus == 80 ~ power + 0.05
      )
    ),
    data = filter(access_sims_plot, n_clus %in% c(40, 80)),
    size = 2) +
  scale_y_continuous(name = "Power\n",
                     limits = c(0, 1.08),
                     breaks = c(.8, .5, 0, 1)) +
  scale_x_continuous(name = "\nNumber of clusters", breaks = unique(access_sims_plot$n_clus)) +
  labs(
    title = "Power simulations for Access Challenge Evaluation",
    caption = "Note: Each value based on 500 simulations across 100 bootstraped samples generated using the DeclareDesign package in R."
  ) +
  theme_bw() + 
  theme(
    strip.background = element_blank(), 
    panel.grid = element_blank(), #element_line(color = '#f5f5f5'),
    legend.position = "bottom",
    text = element_text(family = "Palatino")
  )
dev.off()
