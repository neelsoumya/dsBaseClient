#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.dim::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.dim::smk")
test_that("simple dim, both", {
    dim.res <- ds.retStr('thisishello')

})

context("ds.dim::smk")
test_that("simple dim, split", {
    dim.res <- ds.dim('D', type='split')

    expect_length(dim.res, 3)
    expect_length(dim.res$`dimensions of D in sim1`, 2)
})

context("ds.dim::smk")
test_that("simple dim, combine", {
    dim.res <- ds.dim('D', type='combine')

    expect_length(dim.res, 1)
    expect_length(dim.res$`dimensions of D in combined studies`, 2)
})

#
# Done
#

context("ds.dim::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

context("ds.dim::smk::done")
