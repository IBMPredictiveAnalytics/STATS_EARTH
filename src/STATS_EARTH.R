#/***********************************************************************
# * (C) Copyright Jon K Peck, 2024
# ************************************************************************/

# version 1.0.0

# history
# Mar-2025    Initial version



# helpers
gtxt <- function(...) {
    return(gettext(...,domain="STATS_EARTH"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_EARTH"))
}

loadmsg = "The R %s package is required but could not be loaded."
tryCatch(suppressWarnings(suppressPackageStartupMessages(library(earth, warn.conflicts=FALSE))), error=function(e){
    stop(gtxtf(loadmsg,"partykit"), call.=FALSE)
}
)

mylist2env = function(alist) {
    env = new.env()
    lnames = names(alist)
    for (i in 1:length(alist)) {
        assign(lnames[[i]],value = alist[[i]], envir=env)
    }
    return(env)
}

Warn = function(procname, omsid) {
    # constructor (sort of) for message management
    lcl = list(
        procname=procname,
        omsid=omsid,
        msglist = list(),  # accumulate messages
        msgnum = 0
    )
    # This line is the key to this approach
    lcl = mylist2env(lcl) # makes this list into an environment
    
    lcl$warn = function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        # Accumulate messages and, if dostop or no message, display all
        # messages and end procedure state
        # If dostop, issue a stop.
        
        if (!is.null(msg)) { # accumulate message
            assign("msgnum", lcl$msgnum + 1, envir=lcl)
            # There seems to be no way to update an object, only replace it
            m = lcl$msglist
            m[[lcl$msgnum]] = msg
            assign("msglist", m, envir=lcl)
        } 
        
        if (is.null(msg) || dostop) {
            spssdata.CloseDataConnection()
            lcl$display(inproc)  # display messages and end procedure state

            if (dostop) {
                stop(gtxt("End of procedure"), call.=FALSE)  # may result in dangling error text
            }
        }
    }
    
    lcl$display = function(inproc=FALSE) {
        # display any accumulated messages as a warnings table or as prints
        # and end procedure state, if any
        
        if (lcl$msgnum == 0) {   # nothing to display
            if (inproc) {
                spsspkg.EndProcedure()
                procok = TRUE
            }
        } else {
            procok = inproc
            if (!inproc) {
                procok =tryCatch({
                    StartProcedure(lcl$procname, lcl$omsid)
                    procok = TRUE
                },
                error = function(e) {
                    prockok = FALSE
                }
                )
            }
            if (procok) {  # build and display a Warnings table if we can
                table = spss.BasePivotTable("Warnings and Messages","Warnings", isSplit=FALSE) # do not translate this
                rowdim = BasePivotTable.Append(table,Dimension.Place.row,
                                               gtxt("Message Number"), hideName = FALSE,hideLabels = FALSE)

                for (i in 1:lcl$msgnum) {
                    rowcategory = spss.CellText.String(as.character(i))
                    BasePivotTable.SetCategories(table,rowdim,rowcategory)
                    BasePivotTable.SetCellValue(table,rowcategory,
                                                spss.CellText.String(lcl$msglist[[i]]))
                }
                spsspkg.EndProcedure()   # implies display
            } else { # can't produce a table
                for (i in 1:lcl$msgnum) {
                    print(lcl$msglist[[i]])
                }
            }
        }
    }
    return(lcl)
}


casecorrect = function(vlist, vardict, warns) {
    # correct the case of variable names
    # vlist is a list of names, possibly including TO and ALL
    # vardict is a variable dictionary
    # unrecognized names are returned as is as the GetDataFromSPSS api will handle them

    dictnames = vardict["varName",]
    names(dictnames) = tolower(dictnames)
    dictnames['all'] = "all"
    dictnames['to'] = "to"
    correctednames = list()
    for (item in vlist) {
        lcitem = tolower(item)
        itemc = dictnames[[lcitem]]
        if (is.null(itemc)) {
            warns$warn(gtxtf("Invalid variable name: %s", item), dostop=TRUE)
        }
        correctednames = append(correctednames, itemc)
    }
    return(correctednames)
}
adjustlinear = function(vardict, indvars, linearvars, warns) {
    # return adjusted indvars list
    
    # vardict is an SPSS variable dictionary
    # indvars is the independent case-corrected variable list
    # linearvars is the list of case-corrected linear-entry variables
    
    if (length(linearvars) == 0) {
        return(indvars)  # nothing to do
    }        
    toadd = setdiff(linearvars, indvars)  # linear vars not in indvars
    # fail if non-scale variables
    ###save(linearvars, vardict, file="c:/temp/linear.rdata")
    for (v in linearvars) {
        vl = vardict[which(vardict['varName',] == v)]["varMeasurementLevel",]
        if (vl != "scale") {
            warns$warn(gtxtf("Linear variables must be scale level: %s", v), dostop=TRUE)
        }
    }
    return(c(indvars, toadd))
}


getextloc = function() {
    # find where extensions are installed
    
    rantag = runif(1, 0.05, 1)
    cmd = sprintf('preserve.
    set olang=english.
    oms select tables /if subtypes=["System Settings"]
    /destination format=oxml xmlworkspce="%s", viewer=no.
    show ext.
    omsend.
    restore.', rantag)

    spsspkg.Submit(cmd)
    
    pth = '//pivotTable//group[@text="EXTPATHS EXTENSIONS"]//category[@text="Setting"]/cell/@*'
    res <- spssxmlworkspace.EvaluateXPath(rantag, context="/", pth)
    spssxmlworkspace.DeleteXmlWorkspaceObject(rantag)
    return(res[1])
}


procname=gtxt("Earth")
warningsprocname = gtxt("Earth Warnings")
omsid="STATSEARTH"
warns = Warn(procname=warningsprocname,omsid=omsid)


# main worker

doearth<-function(depvar=NULL, indvars=NULL, linearvars=NULL, idvar=NULL, family="gaussian",
    estimation=TRUE, prediction=FALSE, modelsource=NULL, savemodel=NULL,
    bgcolor="white", degree=1, nfold=0, maxterms=1000,
    modelplots=TRUE, responseplots=TRUE, rptype="model", varimpplot=TRUE,
    height=8, width=8, fontsize=12,
    preddataset=NULL, predtype="link", preddata="training", ignorethis=TRUE
    ) {

    domain<-"STATS_EARTH"
    setuplocalization(domain)
    
    weightvar = spssdictionary.GetWeightVariable()
    if (!is.null(weightvar)) {
        warns$warn(gtxt("Case weights are ignored by this procedure"), dostop=FALSE)
    }
    if (family == "gamma") {
        family = "Gamma"
    }
    vardict = spssdictionary.GetDictionaryFromSPSS()
    if (!is.null(modelsource)) {
        estimation=FALSE
    }
    if (!is.null(preddataset)) {
        if (!is.null(modelsource)) {
            tryCatch(
                load(modelsource),
                error = function(e) {
                    warns$warn(paste(gtxtf("error loading model file: %s", modelsource), e, sep="\n"), 
                        dostop=TRUE)
                }
            )
            depvar = researth$spss$depvar   # used in plotting
            warns$warn(gtxtf("Model loaded from file: %s and will be used for any requested plots and predictions",
                modelsource), dostop=FALSE)
            if (!is.null(savemodel)) {
                warns$warn(gtxt("Model will not be resaved"), dostop=FALSE)
                savemodel = NULL
            }
            idvar = researth$spss[['idvar']]
        }
        if (is.null(idvar)) {
            warns$warn(gtxt("An ID variable must be specified if predictions are requested"), dostop=TRUE)
        }
        idvar = casecorrect(list(idvar), vardict, warns)
        datasetlist = spssdata.GetDataSetList()
        if (tolower(preddataset) %in% tolower(datasetlist)) {
            warns$warn(gtxt("The prediction dataset specified already exists.  Please close it or choose a different name"), 
                       dostop=TRUE)
        }
        if ("*" %in% datasetlist) {
            warns$warn(gtxt("The input dataset must have a name if doing predictions.  Please assign one and rerun the procedure."),
                       dostop=TRUE)
        }
    }

    
    spsspkg.StartProcedure(gtxt("earth"),"STATS EARTH")
    if (estimation) {
        if (is.null(depvar) || is.null(indvars)) {
            warns$warn(gtxt("Dependent and independent variables must be specified if estimating"),
                dostop=TRUE)
        }
        nsplitvars = length(spssdata.GetSplitVariableNames())
        if (nsplitvars > 0) {
            warns$warn(gtxt("Split files is not supported by this procedure"), dostop=TRUE)
        }
        # correct variable name case
        
        depvar = unlist(casecorrect(list(depvar), vardict, warns))
        indvars = casecorrect(indvars, vardict, warns)
        linearvars = casecorrect(linearvars, vardict, warns)
        indvars = adjustlinear(vardict, indvars, linearvars, warns) # indvars will include linear

        if (length(intersect(depvar, indvars)) > 0) {
            warns$warn(gtxt("The dependent variable appears in the independent variable list"),
                dostop=TRUE)
        }
        indvarsplus = paste(indvars, collapse="+")
        f = paste(depvar, indvarsplus, sep="~", collapse="")
        ###save(indvars, depvar, indvarsplus, f, file="c:/temp/frml.rdata")
        f = as.formula(f)
        allvars = c(depvar, indvars)

        # get data api requires case match
        tryCatch(
            {
            dta = spssdata.GetDataFromSPSS(allvars, missingValueToNA=TRUE, factorMode="levels",
                keepUserMissing=FALSE, row.label=unlist(idvar))
            },
            error=function(e) {warns$warn(paste(gtxt("error fetching data"), e, sep="\n"), dostop=TRUE)}
        )
        # earth does not support missing values for estimation
        dta = dta[complete.cases(dta),]
        gc()
        ncases = nrow(dta)
        if (ncases == 0) {
            warns$warn(gtxt("There are no complete cases in the data"), dstop=TRUE)
        }
    tryCatch(
        {
        # linear varnames must end with $, because earth uses s regex on them against indvars
        if (length(linearvars) > 0) {
            lps = c(paste(linearvars, "$", sep=""))
        } else {
            lps = c()
        }
        ###save(f, dta, lps, family, file="c:/temp/est.rdata")
        if (family != "none") {
            researth <<- 
                earth(formula=f, data=dta, linpreds=lps,
                nfold = nfold,
                glm = list(family = family),
                degree = degree,
                nk = maxterms)
        } else {  # no GLM
            researth <<- 
                earth(formula=f, data=dta, linpreds=lps,
                      nfold = nfold,
                      degree = degree,
                      nk = maxterms)
        }
        }, error = function(e) {warns$warn(paste(gtxt("error estimating equation"), e, sep="\n"), dostop=TRUE)}
    )
        
    displaytables(researth, depvar, indvars, ncases, nfold, degree, maxterms, 
        family, preddataset, savemodel)

    # save model with additional information
    if(!is.null(savemodel)) {
        researth$spss = list(depvar=depvar, indvars=indvars, idvar=idvar, estdate=date())
        save(researth, file=savemodel)
        warns$warn(gtxtf("Estimated model saved to file %s", savemodel), dostop=FALSE)
    }
    }
     
    # plots   
    if (!exists("researth")) {
        warns$warn(gtxt("There is no estimated model to use for plots."), dostop=TRUE)
    }

    plotfiles = list()
    dvfactor = is.factor(dta[[depvar]])
    plots = ncol(data.frame(researth$coefficients))
    if (is.null(plots)) {
        plots = list(depvar)
    }
    nplots = max(length(plots), 1)   # always 1 for scale dv's
    for (p in 1:nplots) {
        ###save(researth, dvfactor, p, bgcolor, rptype, file="c:/temp/eplots.rdata")
        if (modelplots) {
            thisplotfile = displayplot(researth, dvfactor, p, height, width,
                fontsize, bgcolor, "modelplots", rptype)
            plotfiles = append(plotfiles, thisplotfile)  # for file cleanup later
        }
        if (responseplots) {
            thisplotfile = displayplot(researth, dvfactor, p, height, width,
                fontsize, bgcolor, "responseplot", rptype)
            plotfiles = append(plotfiles, thisplotfile)  # for file cleanup later
        }
        if (varimpplot) {
            thisplotfile = displayplot(researth, dvfactor, p, height, width,
                fontsize, bgcolor, "varimpplot", rptype)
            plotfiles = append(plotfiles, thisplotfile)
        }
    }
    
    # Insert charts into Viewer.  XML schema does not allow lists of quoted strings,
    # so we have to do this one by one.
    # Insertion in reverse order makes it easier to keep track.
    # The SPSS R api does not provide the api to insert a chart explicitly, so we do it
    # via a tiny extension command written in Python.
    # Submit can't be used within procedure state
    
    spsspkg.EndProcedure()


    if (length(plotfiles) > 0) {
        pfilelist = tempfile("earthplots", tmpdir=tempdir(), fileext=".txt")
        f = file(pfilelist, open="w")
        for (line in plotfiles) {
            writeLines(line, f)
            print(f)
        }
        close(f)

        outlinelabel = sprintf("Variable: %s.  Chart ", paste(depvar, collapse=" "))
        labelparm = list()
        ###cmd = sprintf("STATS INSERT CHART CHARTLIST='%s' HEADER='Earth' OUTLINELABEL='%s ' LABELPARM = %s HIDELOG=%s", 
        cmd = sprintf("STATS INSERT CHART CHARTLIST='%s' HEADER='Earth' OUTLINELABEL='%s ' HIDELOG=%s", 
            pfilelist, outlinelabel, TRUE)
    }
    spsspkg.Submit(cmd)
    
    if (prediction) {
        dopred(researth, preddataset, predtype, idvar, row.names(dta), depvar, 
               is.factor(dta[[depvar]]), family, preddata)
    }
    warns$display(inproc=FALSE)
}


displaytables = function(researth, depvar, indvars, ncases, nfold, degree, 
    maxterms, family, preddataset, savemodel) {
    # display all the tables
    
    displayparameters(researth, depvar, indvars, ncases, nfold, degree, 
        maxterms, family, preddataset, savemodel)
    
    # summary table
    ss = summary(researth)
    srss = ss$rss.per.response
    sgcv = ss$gcv.per.response
    srsq = ss$rsq.per.response
    sgrsq = ss$grsq.per.response
    scvrsq = ss$cv.rsq.tab['mean', ncol(ss$cv.rsq.tab)]
    
    ssdf = data.frame(sgcv, srss, sgrsq, srsq)
    ###colnames(ssdf) = list("GCV", "RSS", "GRSQ", "RSQ") and Csvrsq
    colnames(ssdf) = list(gtxt("Generalized Cross Validation"), 
        gtxt("Residual Sum of Squares"), gtxt("Standardized GCV"), gtxt("R Squared"))
    if (nrow(ssdf) == length(ss$levels)) {
        row.names(ssdf) = ss$levels   # no levels if scale dv
    }
    if (!is.null(scvrsq)) {
        ssdf[ncol(ssdf) + 1] = scvrsq
        colnames(ssdf)[[ncol(ssdf)]] = gtxt("Mean CV Rsq")
    }
    spsspivottable.Display(
        ssdf, 
        title=gtxt("Fit Summary"),
        outline=gtxt("Summary"),
        templateName = "STATSEARTHSUMMARY",
        caption=sprintf(gtxtf("Dependent variable: %s", depvar))
    )
    
    # coefs
    # coefficients is all columns. coef just gives first.  row names are the transformed variables
    coefsdf = data.frame(researth$coefficients)
    spsspivottable.Display(
        coefsdf, 
        title=gtxt("Coefficients"),
        outline=gtxt("Earth Coefficients"),
        templateName="STATSEARTHCOEF",
        caption=sprintf(gtxtf("Dependent variable: %s
h( ) is the hinge function.  See dialog help", depvar))
    )
    
    # varimp nsubsets, gcv, rss
    varimps = data.frame(unclass(evimp(researth)))[c(3, 4 ,6)]
    cnames = list(gtxt("subsets Including Variable"), gtxt("Generalized Cross Validation Reduction"),
        gtxt("Residual Sum of Squares Reduction"))
    colnames(varimps) = cnames
    spsspivottable.Display(
        varimps, 
        title=gtxt("Variable Importance"),
        outline=gtxt("Variable Importance"),
        templateName="STATSEARTHVARIMP",
        caption=sprintf(gtxtf("Dependent variable: %s", depvar)
        )
    )
}


displayparameters = function(researth, depvar, indvars, ncases, nfold, degree, 
    nk, family, preddataset, savemodel) {
    # display parameters and input statistics

    labels = list(
        gtxt("Dependent Variable"),
        gtxt("Independent Variables"),
        gtxt("Maximum Interaction Degree Allowed"),
        gtxt("Number of Cross-Validation Folds"),
        gtxt("Distribution Family"),
        gtxt("Maximum Terms"),
        gtxt("Estimation Date"),
        gtxt("Number of Complete Cases"),
        gtxt("Prediction Dataset"),
        gtxt("Saved Model File")
    )
    values = list(
        depvar,
        paste(indvars, collapse=", "),
        degree, 
        nfold,
        family,
        nk,
        date(),
        ncases,
        ifelse(is.null(preddataset), "--", preddataset),
        ifelse(is.null(savemodel), "--", savemodel)
    )

    df = cbind(values)
    df = data.frame(df, row.names=labels)
    spsspivottable.Display(
        df, 
        title=gtxt("Earth Parameters"),
        outline=gtxt("Earth Parameters"),
        templateName="STATSEARTHPARMS",
        caption=gtxtf("Computations from R earth package, version %s", packageVersion("earth"))
    )
}


displayplot = function(researth, dvfactor, nresponse, height, width,
    fontsize, bgcolor, ptype, rptype) {

    # researth is the model
    # nresponse is the category for a categorical dv or blank
    # dvfactor is TRUE if depvar is a factor
    # height, width are the size parameters for tree #1
    # dvfactor is true if the (first) dep var is a factor
    # ptype specifies the type of plot to do
    
    if (!dvfactor && rptype == "distribution") {
        warns$warn(gtxt("Distribution plots are not available for scale variables"),
            dostop=FALSE)
        return()
    }
    reso = 72
    
    if (is.null(height)) {
        height = 8
    }

    if (is.null(width)) {
        width = 8
    } 
    
    pfile = tempfile("earthplots", tmpdir=tempdir(), fileext=paste("earthplots",".png", sep=""))
    tryCatch(
        suppressMessages(png(pfile, units="in", res=72, height=height, width=width, bg=bgcolor)),
        error = function(e) {warns$warn(e, dostop=TRUE)}
    )
    drawtheplot(researth, nresponse, fontsize, bgcolor, ptype, rptype)
    return(pfile)
}


drawtheplot = function(result, nresponse, fontsize, plotbg, ptype, rptype) {
    ###save(ptype, result, nresponse, plotbg, file="c:/temp/plotmo.rdata")

    tryCatch(
        {
        # gp = gpar(fontsize = fontsize),

        if (ptype == "modelplots") {
                plot(result, nresponse = nresponse,  
                     bg ="ivory2")
        }
        if (ptype == "responseplot") {
            if (rptype == "model") {
                z=capture.output(plotmo(result, nresponse = nresponse, 
                    bg="ivory2"))
            } else {
                z=capture.output(plotd(result, nresponse = nresponse, 
                    bg="ivory2"))
            }
        }
        if (ptype == "varimpplot") {
            ev = evimp(researth)
            par(bg="ivory2")    
            z = capture.output(plot(ev))
        }
                
            # gp = gpar(fontsize = fontsize), 
            # using capture.Output to suppress some spurious print that
            # can't be suppressed other ways
            ###z=capture.output(plotmo(result, nresponse = nresponse, 
               ###      bg="ivory2"))
        },
        error = function(e) {print(e)
            warns$warn(gtxtf("Plot drawing failure\n%s", e), dostop=FALSE)
        }
    )
    dev.off()
   ###print('plot done')
}

dopred = function(researth, preddataset, predtype, idvar, idvardata, depvar, dvfactor, 
    family, datasource) {

    # create a dataset of predicted values along with the idvar variable
    # If a factor and the family is binomial, there is one column for each depvar value
    
    # Predicting with standard earth models
    # Use the default type="link", or possibly type="class".
    # Actually, the "link", "response", and "earth" choices all return the same value 
    # unless the glm argument was used in the original call to earth.
    # 
    # Predicting with earth-GLM models
    # This section applies to earth models when the glm argument was used in 
    # the original call to earth.
    # In brief: for logistic models use type="response" to get probabilities, 
    # and type="link" to get log-odds.
    # 
    # not supported Use option "earth" to get the linear fit 
    # (this gives the prediction you would get if your original call to earth 
    #     had no glm argument).
    # 
    # Predicting with "class"
    # Use option "class" to get the predicted class. 
    # with option "class", this function first makes predictions with type="response" 
    # and then assigns the predicted values to classes as follows:
    # (i) When the response is a logical, 
    # predict TRUE if the predicted probability is greater than thresh (default 0.5).
    # 
    #(ii) When the response is a numeric, 
    # predict TRUE if the predicted value is greater than thresh. 
    # Actually, this is identical to the above case, although thresh here may 
    # legitimately be a value outside the 0...1 range.
    #
    #(iii) When the response is a two level factor, 
    # predict the second level if its probability is more than thresh. 
    # In other words, with the default thresh=0.5 predict the most probable level.
    # 
    #(iv) When the response is a three or more level factor, 
    # predict the most probable level (and thresh is ignored).
    
    # predtypes
    # link: for logistic gives log odds
    # response: for logistic, gives probabilities
    # class - factors only
    
    if (predtype == "class" && !dvfactor) {
        warns$warn(gtxt("Prediction type class is only for factors"), dostop=TRUE)
    }
    # dict record is varName, varLabel, varType, varFormat, varMeasurementLevel
    inputdict = spssdictionary.GetDictionaryFromSPSS(c(idvar, depvar))
    if (datasource == "training") {
        preds = predict(researth, type=predtype)
    } else {
        # get new data
        depvar = researth$spss[['depvar']]
        indvars = researth$spss[['indvars']]  # includes linearvars
        idvar =  researth$spss[['idvar']]
        tryCatch(
            {
            # all the independent vars actually used
            pvars = attr(researth$terms, "term.labels")
            dta = spssdata.GetDataFromSPSS(c(pvars), missingValueToNA=TRUE, 
                factorMode="levels", keepUserMissing=FALSE, row.label=unlist(idvar))
            },
            error=function(e) {warns$warn(paste(gtxt("error fetching new prediction data"), e, sep="\n"), 
                dostop=TRUE)}
        )

        idvardata = row.names(dta)
        warns$warn(gtxt("Data source is new data"), dostop=FALSE)
        preds = predict(researth, type=predtype, newdata=dta)
    }
    ncols = ncol(preds)
    idinfo = inputdict[, inputdict['varName', ] == idvar]
    dvinfo =  inputdict[, inputdict['varName', ] == depvar]
    dictlist = list()
    dictlist[[1]] = idinfo
    ###save(idinfo, dvinfo, preds, file="c:/temp/dictinfo.rdata")
    colnames(preds) = fixnames(colnames(preds))
    ###spsspkg.EndProcedure()
    cn = colnames(preds)

    for (col in 1: ncol(preds)) {  # todo: response types
        varspec = list()
        varspec[[1]] = cn[[col]]  # var name
        varspec[[2]] = ""
        if (!dvfactor || predtype == "class") {
            varspec[[3]] = dvinfo[[3]]
            varspec[[4]] = dvinfo[[4]]
            varspec[[5]] = dvinfo[[5]]
        } else {
            varspec[[3]] = 0
            varspec[[4]] = "F8.3"
            varspec[[5]] = "scale"
        }
         dictlist[[col+1]] = unlist(varspec)
    }
    dict = spssdictionary.CreateSPSSDictionary(dictlist)
    preds = data.frame(cbind(idvardata, preds))
    ###save(preds, dict, dictlist, idvardata, preddataset, file="c:/temp/dataset.rdata")
    tryCatch(
        {
        spssdictionary.SetDictionaryToSPSS(preddataset, dict)

        spssdata.SetDataToSPSS(preddataset, preds) #row.names(preddata)?
        spssdictionary.EndDataStep()
        }
    )
    
}

# subpunct approximates characters invalid in SPSS variable names
subpunct = "[-’‘%&'()*+,/:;<=>?\\^`{|}~’]"
fixnames = function(names) {
    # return list of legal, nonduplicative SPSS variable names for the input list
    
    # dta is a list/vector of names to correct
    # this function may not perfectly match SPSS name rules
    
    newnames = c()
    for (name in names) {
        newname = gsub(subpunct, "_", name)   # eliminate disallowed characters
        newname = gsub("(^[0-9])", "X_\\1", newname)  # fix names starting with digit
        newname = gsub("^\\.|\\.$", "_", newname)  # fix names starting or ending with "."
        # }
        # ensure that there are no duplicate names
        basename = newname
        for (i in 1:1000) {
            if (!(newname %in% newnames)) {
                break
            } else {
                newname = paste(basename, i, sep="_")
            }
        }
        newnames = append(newnames, newname)
    }
    return(newnames)
}

setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    if (!is.null(fpath)) {
        bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
    }
} 


Run<-function(args){

    cmdname = args[[1]]
    args <- args[[2]]

    # variable keywords are typed as varname instead of existingvarlist in
    # order to allow for case correction of names later, since the data fetching apis are
    # case sensitive

    oobj <- spsspkg.Syntax(templ=list(
        spsspkg.Template("DEPVAR", subc="", ktype="varname", var="depvar", islist=FALSE),
        spsspkg.Template("INDVARS", subc="", ktype="varname", var="indvars", islist=TRUE),
        spsspkg.Template("LINEARVARS", subc="", ktype="varname", var="linearvars", islist=TRUE),
        spsspkg.Template("IDVAR", subc="", ktype="varname", var="idvar", islist=FALSE),
        spsspkg.Template("ESTIMATE", subc="", ktype="bool", var="estimation",islist=FALSE),
        spsspkg.Template("PREDICT", subc="", ktype="bool", var="prediction",islist=FALSE),
        spsspkg.Template("FAMILY", subc="", ktype="str", var="family", islist=FALSE,
            vallist=list("gaussian", "binomial", "poisson", "gamma", "none")),
        spsspkg.Template("PREDDATASET", subc="", ktype="varname", var="preddataset", islist=FALSE),
        spsspkg.Template("PREDTYPE", subc="", ktype="str", var="predtype",
            vallist=list("link", "response", "class"), islist=FALSE),
        spsspkg.Template("MODELSOURCE", subc="", ktype="literal", var="modelsource", islist=FALSE),
        spsspkg.Template("DATASOURCE", subc="", ktype="str", var="preddata", 
            vallist=list("training", "newdata"), islist=FALSE),

        spsspkg.Template("SAVEMODEL", subc="", ktype="literal", var="savemodel", islist=FALSE),
        
        spsspkg.Template("NFOLD", subc="OPTIONS", ktype="int", var="nfold", islist=FALSE),
        spsspkg.Template("DEGREE", subc="OPTIONS", ktype="int", var="degree", islist=FALSE),
        spsspkg.Template("MAXTERMS", subc="OPTIONS", ktype="int", var="maxterms", islist=FALSE,
                         vallist=list(1, 1000)),
        spsspkg.Template("IGNORETHIS", subc="OPTIONS", ktype="str", var="ignorethis", islist=FALSE),
                
        spsspkg.Template("MODELPLOTS", subc="DISPLAY", ktype="bool", var="modelplots", islist=FALSE),
        spsspkg.Template("RESPONSEPLOTS", subc="DISPLAY", ktype="bool", var="responseplots", islist=FALSE), 
        ###spsspkg.Template("RPTYPE", subc="DISPLAY", ktype="str", var="rptype",
            ###vallist=list("model", "distribution"), islist=FALSE),
        spsspkg.Template("VARIMPPLOT", subc="DISPLAY", ktype="bool", var="varimpplot", islist=FALSE), 
        
        spsspkg.Template("HEIGHT", subc="DISPLAY", ktype="float", var="height", islist=FALSE),
        spsspkg.Template("WIDTH", subc="DISPLAY", ktype="float", var="width", islist=FALSE),
        spsspkg.Template("FONTSIZE", subc="DISPLAY", ktype="float", var="fontsize", islist=FALSE),
        spsspkg.Template("BGCOLOR", subc="DISPLAY", ktype="literal", var="bgcolor", islist=FALSE)
        ))

    if ("HELP" %in% attr(args,"names"))
        #writeLines(helptext)
        helper(cmdname)
    else {
        res <- spsspkg.processcmd(oobj, args, "doearth")
    }
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

    if (exists("spsspkg.helper")) {
        assign("helper", spsspkg.helper)
    }
}
