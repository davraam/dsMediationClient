#-------------------------------------------------------------------------------
# Copyright (c) 2018-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.mediate::smk_expt::setup")

connect.mediation.dataset.upb(list('att', 'attbin', 'negaff', 'age', 'gender', 'educ', 'UPB'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.mediate::smk_expt::setup::fits")
test_that("setup mediate", {
    med.fit <- ds.glmSLMA(formula = 'negaff ~ att + gender + educ + age', family = 'gaussian', dataName = 'D', newobj = 'med.fit')

    expect_length(med.fit, 9)
    expect_equal(med.fit$validity.check, "<med.fit> appears valid in all sources", fixed = TRUE)

    out.fit <- ds.glmSLMA(formula = 'UPB ~ negaff + att + gender + educ + age', family = 'binomial', dataName = 'D', newobj = 'out.fit')

    expect_length(out.fit, 9)
    expect_equal(out.fit$validity.check, "<out.fit> appears valid in all sources", fixed = TRUE)
})

context("ds.mediate::smk_expt::sims=10")
test_that("mediate sims 10", {
    med.out <- ds.mediate(model.m = 'med.fit', model.y = 'out.fit', treat = "att", mediator = "negaff", boot = FALSE, conf.level = 0.95, robustSE = TRUE, sims = 10, seed = 123)

    expect_equal_to_reference(med.out, 'smk_expt-results/ds.mediate_sims10.rds')
})

context("ds.mediate::smk_expt::sims=100")
test_that("mediate sims 100", {
    med.out <- ds.mediate(model.m = 'med.fit', model.y = 'out.fit', treat = "att", mediator = "negaff", boot = FALSE, conf.level = 0.95, robustSE = TRUE, sims = 100, seed = 123)

    expect_equal_to_reference(med.out, 'smk_expt-results/ds.mediate_sims100.rds')
})

#
# Done
#

context("ds.mediate::smk_expt::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "med.fit", "out.fit", "med.out"))
})

disconnect.mediation.dataset.upb()

context("ds.mediate::smk_expt::done")
