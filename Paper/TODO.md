# Mine

 * Plot regression line in onset/offset analysis
 * clarify all years analysis in figure captions
 * Put 1-2% punchline in the abstract, maybe
 * So it turns out that several regimens are actually the same, and that the best regional regimen @ 2w rounding is actually the same as the best statewide regimen @ 2w rounding. As a result, the conclusion could be simplified to statewide 2w rounding.
 * TODO paragraphs
 
# Dan's

 * We need more detail about how the fitted model is used to calculate onset and offset dates--maybe even a supplementary diagram that shows an observed curve, a fitted curve, and the cumulative curve with thresholds marked on each
 * Need to discuss where this is going journal-wise. We need to emphasize a bit more that what we are interested in here is the determination of what spatial scale is relevant for clinical decision making for prophylaxis. 
 * On R package, the language is a bit confusing. What do you mean by ‘potential outbreak’? Also ‘outbreak’ is too narrow. RSV is a seasonally endemic disease. I wouldn’t call the seasonal peaks ‘outbreaks’
 * (R package docs) “Predict cumulative case count for an outbreak”...not really I don’t think--you are fitting a curve and then summing simulated values from this model, right?
 * [x] Abstract: Methods section needs to be stated more clearly to make it clear what the goal was. Such as “We obtained the count of the number of weekly hospital admissions with a diagnostic code for RSV at the state and country levels in Connecticut for July 1995-June 2013. We fit a penalized cubic spline  through these data to obtain a smoothed seasonal curve for each location and used these smoothed curves to calculate the fraction of cases of RSV that occur during the period of protection conferred by immunoprophylaxis, according to the American Academy of Pediatrics guidelines. We also considered the protection that would be conferred by several alternative immunoprophylaxis regimens that have the same number of doses (ie if prophylaxis was started earlier or later).
 * [x] Introduction: Ditch subheadings--this wouldn’t be consistent with style in most journals
 * [x] Intro: Prophylaxis section: need to state at the beginning who receives prophylaxis.
 * Intro: seasonality section: need to explain more about the seasonal variation in timing--FL isn’t the only state that is off. There is a clear wave-like pattern starting in SE, and then moving N and W. Also starting earlier in urban, then later in rural 
 * [x] Not sure this paragraph is useful: Spatio-temporal variability of RSV Nationwide spatial variability of RSV season has been established by the Centers for Disease Control and Prevention (CDC).12,13
 * [x] Intro: paragraph starting “In Connecticut”: this should be moved to methods--first paragraph, right before case definitions.
 * “Study Aims: these are good--probably just reformat it to be in paragraph form rather than list
 * [x] Paragraph on “The study was approved by…” this should be in methods, right after description of hospitalization data
 * [x] “Data normalization paragraph”. This needs to be stated more simply. Just say that because epidemics peak cross multiple calendar years, we define the epidemiological years as running from the beginning of July to the following June. 
 * [x] Exclusions: I think the change in diagnostic coding was in 1997, so we should exclude data before July 1997 (Nope, see summary plot in RSV.Rmd)
 * [x] “Choice of model”: this is probably too technical of a description for a clinical journal. Need to state more simply that we fit a smoothing function through the seasonal data, and this allows us to calculate season onset and offset, and we can use resampling methods to calculate CI. Then we can have a supplement where we explain in mre technical detail
 * Need to be consistent about start date of data. In some places say 1995, in some 1996 (and we might want it to be 1997)
 * The methods section seems quite long--some of these details might want to go in a supplement
 * Figure order. It seems like maybe Fig 4 should come earlier? It is a nice visual and could help explain fig 2 and fig 3.
 * ‘’Sptially-adjusted regimens are superior’...it seems this is still true after rounding to weekly or biweekly levels, but not monthly 
 * ‘RSV season is shifting earlier’ This is an interesting observation--I might save it for a discussion point though--there is already a lot in the results, especially because temporally adjusting regimen doesn’t make it better.
 * It might be nice to do a back-of-the-envelope calculation for how many cases we might expect to prevent by shifting the calendar. Ie what does a 2% improvement in coverage translate to in terms of high-risk cases averted We would need an estimate of how many kids are on prophylaxis in the state and the effectiveness of a 5 dose schedule.

# Ginny's

