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

context("ds.retStr::smk::setup")

# connect.studies.dataset.cnsim(list("LAB_TSC"))
connect.studies.dataset.survival(list("survtime","time.id","female","age.60"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#connect.studies.dataset.survival(list("D"))
##init.studies.dataset.survival(list("D"))
#init.studies.dataset.survival_nomissing(list("D"))
##connect.studies.dataset.dasim(c("SURVTIME"))
## add server side survival variables
#ls_object <- add_server_side_var_survival()
#print(ls_object)

#
# Tests
#


context("ds.retStr::smk")
test_that("simple ds.retStr call", {
    dim.res <- ds.retStr('thisishello')
    
    expect_match(as.character(dim.res), 'hello', ignore.case = TRUE)

})



# testthat::expect_error( as.character(ds.retStr('1==1') ) )
              
context("ds.coxphSLMA::smk")
test_that("simple error, SQL injection", {
    
    #try( cox_object <- ds.coxph.SLMA(formula = 'survival::Surv(time=SURVTIME,event=EVENT)~D$age.60')#, dataName = 'D') 
    #, silent = FALSE)
    
    surv_object <- ds.Surv(time_param = 'SURVTIME', event_param = 'EVENT', objectname = 'surv_object')
    try(cox_object <- ds.coxph.SLMA(formula = 'surv_object~AGE')#, dataName = 'D')
    , silent=FALSE)
    # print(cox_object$study1$call)
    
    # print(cox_object$study1$coefficients[1,1])
    
    print( datashield.errors() )
    
    # summary(cox_object)
    
    expect_error( as.character(  ds.coxph.SLMA(formula = 'survival::Surv(time=SURVTIME,event=EVENT)~D$age', dataName = 'D')   ) )
})

cat("hello")



#
# Done
#

context("ds.dim::smk::shutdown")

#test_that("shutdown", {
#    ds_expect_variables(c("D"))
#})

disconnect.studies.dataset.cnsim()
disconnect.studies.dataset.survival()

context("ds.dim::smk::done")
