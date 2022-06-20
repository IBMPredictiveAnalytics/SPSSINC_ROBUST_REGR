#/***********************************************************************
# * Licensed Materials - Property of IBM 
# *
# * IBM SPSS Products: Statistics Common
# *
# * (C) Copyright IBM Corp. 1989, 2014, 2022
# *
# * US Government Users Restricted Rights - Use, duplication or disclosure
# * restricted by GSA ADP Schedule Contract with IBM Corp. 
# ************************************************************************/

# history
# 28-feb-2014  original version
# 20-jun-2o22  use factor values instead of labels


helptext="The SPSSINC ROBUST REGR command requires the R Integration Plug-in
and the R MASS package (part of the VR bundle).

SPSSINC ROBUST REGR DEPENDENT=dependent variable 
ENTER=independent variables
[/OPTIONS [MISSING={LISTWISE**}] [EXECUTE={TRUE**}] ]
                   {FAIL      }           {FALSE }
[/SAVE [RESIDUALSDATASET=datasetname] [COEFSDATASET=datasetname]
       [PROGRAMFILE=filespec] ]

Split files and weight are not honored by this command.

SPSSINC ROBUST REGR /HELP prints this information and does nothing else.

Example:
SPSSINC ROBUST REGR DEPENDENT=mpg ENTER=engine weight.

Execute the rlm function for robust regression from the R MASS package.
DEPENDENT and ENTER specify the dependent and independent
variable names.  

Categorical independent variables are automatically converted
appropriately to factors.  A constant term is automatically included.

MISSING=LISTWISE causes listwise deletion of missing values (but
missings in factors are included as a level).  FAIL stops the procedure if
missing values are encountered.

EXECUTE=FALSE runs the command syntax without running the robust regression.  
This is mainly useful in combination with SAVE PROGRAMFILE.

/SAVE RESIDUALSDATASET causes a dataset containing the residuals to be created.
The dataset name must not already be in use.
The case number is included as cases will only be written for input cases with no
missing data.  Data filtered by IBM SPSS Statistics are not passed to R and will not
generate cases in the residuals dataset.

COEFSDATASET causes a new dataset containing the coefficients to be created.
The dataset name must not already be in use.

PROGRAMFILE causes the R code that implements the robust regression to be written
to the specified file. Since the rlm function has features not exposed in this 
extension command, the generated program can be a useful starting point for 
additional specifications.
"

robust<-function(dep, enter, missing="listwise", residualsdataset=NULL, coefsdataset=NULL){

    domain<-"SPSSINC_ROBUST_REGR"
    setuplocalization(domain)
    
    tryCatch(library(MASS), error=function(e){
        stop(gettextf("The R %s package is required but could not be loaded.","MASS",domain=domain),call.=FALSE)
        }
    )

    if (identical(missing,"listwise")) {missing<-na.exclude} else {missing<-na.fail}

    allvars <- c(dep,enter)
    model <- paste(dep,"~",paste(enter,collapse="+"))

    dta<-spssdata.GetDataFromSPSS(allvars,missingValueToNA=TRUE,factorMode="levels")
    res <- tryCatch(
            summary(resrlm <- rlm(as.formula(model), data=dta, na.action=missing, method="MM", model=FALSE)),
            error=function(e) {return(c(gettext("ERROR:",domain=domain),e))}
           )

    if (!is.null(res$message)) {print(res$message)} else {
        miss<-ifelse(identical(missing,na.exclude),"na.exclude","na.fail")
        caption = paste(paste("rlm(formula = ",model,", data = dta, na.action = ",miss,", method = ",dQuote("MM"),", model = FALSE)",sep=""),
            sprintf(paste("\n",gettext("Residual standard error: ",domain=domain),"%.5f\n",
                        gettext("Degrees of freedom: ",domain=domain),"%s",sep=""), res$sigma, res$df[[2]]))
        
        coeff<-coefficients(res)
        for (i in 1:length(attributes(coeff)$dimnames[[1]])){
           attributes(coeff)$dimnames[[1]][[i]]=gettext(attributes(coeff)$dimnames[[1]][[i]],domain=domain)
        }
        
        for (i in 1:length(attributes(coeff)$dimnames[[2]])){
           attributes(coeff)$dimnames[[2]][[i]]=gettext(attributes(coeff)$dimnames[[2]][[i]],domain=domain)
        }
        
        StartProcedure(gettext("Robust Regression",domain=domain),"SPSSINC ROBUST REGR")
        spsspivottable.Display(coeff, 
            title=gettext("Coefficients",domain=domain), templateName="SPSSINCROBUSTREGR",
            caption=caption,
            isSplit=FALSE)
        spsspkg.EndProcedure()
       
        if (!is.null(residualsdataset)){
            dict<- spssdictionary.CreateSPSSDictionary(c("caseNumber", gettext("Case Number",domain=domain), 0, "F8.0", "nominal"),
            c("rlmResiduals", model, 0, "F8.2", "scale"))
            tryCatch({
                spssdictionary.SetDictionaryToSPSS(residualsdataset, dict)
                df = data.frame(res$residuals)
                spssdata.SetDataToSPSS(residualsdataset, data.frame(row.names(df), res$residuals))
                },
                error=function(e) {print(e)
                cat(gettext("Failed to create residuals dataset. Dataset name must not already exist: ",domain=domain),residualsdataset)
                }
            ) 
        }
        if (!is.null(coefsdataset)){
            dict<- spssdictionary.CreateSPSSDictionary(c("term", gettext("Variable or Factor Value",domain=domain), 100, "A100", "nominal"),
            c("coefficient", gettext("Estimated Coefficient",domain=domain), 0, "F10.3", "scale"))
            tryCatch({
                spssdictionary.SetDictionaryToSPSS(coefsdataset, dict)
                spssdata.SetDataToSPSS(coefsdataset, data.frame(row.names(res$coef), res$coef[,1]))
                },
                error=function(e) {print(e)
                cat(gettext("Failed to create coefficients dataset. Dataset name must not already exist: ",domain=domain),coefsdataset)
                }
            )  
        }
        
        spssdictionary.EndDataStep()
    }       

    res <- tryCatch(rm(list=ls()),warning=function(e){return(NULL)})
}

StartProcedure<-function(procname, omsid){
if (as.integer(substr(spsspkg.GetSPSSVersion(),1, 2)) >= 19)
   spsspkg.StartProcedure(procname,omsid)
else
   spsspkg.StartProcedure(omsid)
}

caller<-function(dep, enter, missing="listwise", residualsdataset=NULL, coefsdataset=NULL,
       programfile=NULL, execute=TRUE){
    
    if(!is.null(programfile)){
        lines<-c("robust<-",
            attr(robust,"source"),
            paste("dep<-",dQuote(dep),sep=""),
            paste("enter<-",deparse(enter),sep=""),
            paste("missing<-",dQuote(missing),sep=""))
        func<-"robust(dep, enter, missing"
        if(!is.null(residualsdataset)){
            func<-paste(func,", residualsdataset=",dQuote(residualsdataset),sep="")
        }
        if(!is.null(coefsdataset)){
            func<-paste(func,", coefsdataset=",dQuote(coefsdataset),sep="")
        }
        func<-paste(func,")",sep="")
        lines<-c(lines,func)        
        f<-file(description=programfile,open="wb",encoding="UTF-8")
        writeLines(lines,con=f)
        close(f)
    }
    
    if (execute) robust(dep, enter, missing, residualsdataset, coefsdataset)
    
}

setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 

Run<-function(args){
    cmdname = args[[1]]
    args <- args[[2]]
    oobj<-spsspkg.Syntax(templ=list(
                spsspkg.Template("DEPENDENT", subc="",  ktype="existingvarlist", var="dep", islist=FALSE),
                spsspkg.Template("ENTER", subc="",  ktype="existingvarlist", var="enter", islist=TRUE),
                spsspkg.Template("MISSING", subc="OPTIONS",ktype="str", var="missing"),
                spsspkg.Template("RESIDUALSDATASET", subc="SAVE", ktype="literal", var="residualsdataset"),
                spsspkg.Template("COEFSDATASET", subc="SAVE", ktype="literal", var="coefsdataset"),
                spsspkg.Template("EXECUTE", subc="OPTIONS", ktype="bool", var="execute"),
                spsspkg.Template("PROGRAMFILE", subc="SAVE", ktype="literal", var="programfile")
                ))

    if ("HELP" %in% attr(args,"names"))
        #writeLines(helptext)
        helper(cmdname)
    else
        res <- spsspkg.processcmd(oobj,args,"caller")
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}