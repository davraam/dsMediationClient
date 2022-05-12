#-------------------------------------------------------------------------------
# Copyright (c) 2019-2021 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("ds.mediate::smk::setup")

connect.mediation.dataset.upb(list('att', 'attbin', 'negaff', 'age', 'gender', 'educ', 'UPB'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.mediate::smk")
test_that("simple mediate", {
    med.fit <- ds.glmSLMA(formula = 'negaff ~ att + gender + educ + age', family = 'gaussian', dataName = 'D', newobj = 'med.fit')

    expect_length(med.fit, 9)
    expect_equal(med.fit$validity.check, "<med.fit> appears valid in all sources", fixed = TRUE)

    out.fit <- ds.glmSLMA(formula = 'UPB ~ negaff + att + gender + educ + age', family = 'binomial', dataName = 'D', newobj = 'out.fit')

    expect_length(out.fit, 9)
    expect_equal(out.fit$validity.check, "<out.fit> appears valid in all sources", fixed = TRUE)

    med.out <- ds.mediate(model.m = 'med.fit', model.y = 'out.fit', treat = "att", mediator = "negaff", boot = FALSE, conf.level = 0.95, robustSE = TRUE, sims = 10, seed = 123, newobj = 'med.out')

    expect_length(med.out, 3)
    expect_equal(class(med.out), "list")
    expect_true(all(c('study1', 'study2', 'study3') %in% names(med.out)))

    expect_length(med.out$study1, 56)
    expect_true(all(c("d0", "d1", "d0.ci", "d1.ci", "d0.p", "d1.p", "d0.sims", "d1.sims","z0", "z1", "z0.ci", "z1.ci","z0.p", "z1.p", "z0.sims", "z1.sims",
                      "n0", "n1", "n0.ci", "n1.ci","n0.p", "n1.p", "n0.sims", "n1.sims","tau.coef", "tau.ci", "tau.p", "tau.sims","d.avg", "d.avg.p", "d.avg.ci", "d.avg.sims",
                      "z.avg", "z.avg.p", "z.avg.ci", "z.avg.sims", "n.avg", "n.avg.p", "n.avg.ci", "n.avg.sims", "boot", "boot.ci.type", "treat", "mediator",
                      "covariates", "INT", "conf.level", "model.y", "model.m", "control.value","treat.value", "nobs", "sims", "call", "robustSE", "cluster") %in% names(med.out$study1)))
    expect_true(all(c("summary.mediate", "mediate") %in% class(med.out$study1)))

    expect_length(med.out$study2, 56)
    expect_true(all(c("d0", "d1", "d0.ci", "d1.ci", "d0.p", "d1.p", "d0.sims", "d1.sims","z0", "z1", "z0.ci", "z1.ci","z0.p", "z1.p", "z0.sims", "z1.sims",
                      "n0", "n1", "n0.ci", "n1.ci","n0.p", "n1.p", "n0.sims", "n1.sims","tau.coef", "tau.ci", "tau.p", "tau.sims","d.avg", "d.avg.p", "d.avg.ci", "d.avg.sims",
                      "z.avg", "z.avg.p", "z.avg.ci", "z.avg.sims", "n.avg", "n.avg.p", "n.avg.ci", "n.avg.sims", "boot", "boot.ci.type", "treat", "mediator",
                      "covariates", "INT", "conf.level", "model.y", "model.m", "control.value","treat.value", "nobs", "sims", "call", "robustSE", "cluster") %in% names(med.out$study2)))
    expect_true(all(c("summary.mediate", "mediate") %in% class(med.out$study2)))

    expect_length(med.out$study3, 56)
    expect_true(all(c("d0", "d1", "d0.ci", "d1.ci", "d0.p", "d1.p", "d0.sims", "d1.sims","z0", "z1", "z0.ci", "z1.ci","z0.p", "z1.p", "z0.sims", "z1.sims",
                      "n0", "n1", "n0.ci", "n1.ci","n0.p", "n1.p", "n0.sims", "n1.sims","tau.coef", "tau.ci", "tau.p", "tau.sims","d.avg", "d.avg.p", "d.avg.ci", "d.avg.sims",
                      "z.avg", "z.avg.p", "z.avg.ci", "z.avg.sims", "n.avg", "n.avg.p", "n.avg.ci", "n.avg.sims", "boot", "boot.ci.type", "treat", "mediator",
                      "covariates", "INT", "conf.level", "model.y", "model.m", "control.value","treat.value", "nobs", "sims", "call", "robustSE", "cluster") %in% names(med.out$study3)))
    expect_true(all(c("summary.mediate", "mediate") %in% class(med.out$study1)))
})

#
# Done
#

context("ds.mediate::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "med.fit", "out.fit", "med.out"))
})

disconnect.mediation.dataset.upb()

context("ds.mediate::smk::done")
