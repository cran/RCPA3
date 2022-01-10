#' States dataset for R Companion to Political Analysis, Third Edition
#'
#' A dataset with variables about the 50 states. This dataset is used to demonstrate application of R to political analysis. See book Appendix for variable names and descriptions.
#'
#' @format A data frame with 50 rows and 149 variables.
#' \describe{
#'   \item{abortion.rate}{Number of abortions per 1000 women 15-44, 2008}
#'   \item{abortlaws}{Number of restrictions on abortion}
#'   \item{abortlaws.3cat}{Restrictiveness of state abortion laws, 3 ordinal categories}
#'   \item{adv.or.more}{Percentage of 25+ population with graduate or professional degree}
#'   \item{alcohol}{Alcohol consumption (gal/capita) 2007}
#'   \item{attend.pct}{Percentage freq attend relig serv (Pew)}
#'   \item{ba.or.more}{Percentage of 25+ population with bachelor's degree or more}
#'   \item{battleground2020}{Battleground in 2020 election? }
#'   \item{biden2020}{Two-party vote share for Biden in 2020 election}
#'   \item{biden2020.ev}{Electoral College votes for Biden in 2020 election}
#'   \item{biz.tax.rank}{State business tax climate ranking}
#'   \item{biz.tax.score}{State business tax climate rating}
#'   \item{black.percent}{Percentage of population black or African American}
#'   \item{black.stateleg}{Percent of state legislators who are African American}
#'   \item{brady.rank}{Brady Campaign ranking}
#'   \item{brady.score}{Brady Campaign score}
#'   \item{broadband}{Percentage of households with broadband Internet subscription}
#'   \item{carfatal}{Motor vehicle fatalities (per 100,000 pop)}
#'   \item{carfatal07}{Motor vehicle fatalities per 100,000 pop (2007)}
#'   \item{cig.tax}{Cigarette tax per pack}
#'   \item{cig.tax.3cat}{Cigarette tax per pack, 3 ordinal categories}
#'   \item{cigarettes}{Packs bimonthly per adult pop}
#'   \item{citizen.ideology}{Citizen ideology index}
#'   \item{clinton2016}{Vote share for Clinton in 2016 election}
#'   \item{cong.dem}{Percentage of state's 2020 congressional delegation that is Democratic}
#'   \item{cook.index}{Higher scores more Dem}
#'   \item{cook.index3}{3 quantiles of cook_index}
#'   \item{corrections.incarc.rate}{Population incarcerated per 100,000 state residents }
#'   \item{corrections.total.rate}{Population under correctional supervision per 100,000 state residents}
#'   \item{covid.cases}{COVID cases (as of June 2021)}
#'   \item{covid.cases.per1000}{COVID cases per 1,000 persons (as of June 2021)}
#'   \item{covid.deaths}{COVID deaths (as of June 2021)}
#'   \item{covid.deaths.per1000}{COVID deaths per 1,000 persons (as of June 2021)}
#'   \item{covid.response.max}{Maximum of COVID response stringency index}
#'   \item{covid.response.mean}{Mean of COVID response stringency index }
#'   \item{covid.vaccinated}{Percentage of population fully vaccinated against COVID (as of June 2021)}
#'   \item{crime.rate.burglary}{Burglary rate, per 100,000 population}
#'   \item{crime.rate.murder}{Murder and non-negligent manslaughter rate, per 100,000 population}
#'   \item{crime.rate.property}{Property crime rates, per 100,000 population}
#'   \item{crime.rate.violent}{Violent crime rate, per 100,000 population}
#'   \item{deathpen.executions}{Executions since 1976}
#'   \item{deathpen.exonerations}{Death penalty exonerations since 1973}
#'   \item{deathpen.status}{Does state retain death penalty?}
#'   \item{defexpen}{Federal defense expenditures per capita}
#'   \item{dem.stateleg}{Percent of state legislators who are Democrats}
#'   \item{density}{Population per square mile}
#'   \item{division}{Census division}
#'   \item{drug.death.rate}{Drug overdose death rate per 100,000 adults}
#'   \item{earmarks.pcap}{Earmarks per capita (in dollars)}
#'   \item{foreign.born}{Percentage of population born outside the United States}
#'   \item{gay.policy}{Billman's policy scale}
#'   \item{gay.policy2}{RECODE of gay_policy (Billman's policy scale)}
#'   \item{gay.policy.con}{Does state have 'most conservative' gay policies?}
#'   \item{gay.support}{Lax-Phillips opinion index}
#'   \item{gay.support3}{Gay rights: public support}
#'   \item{giffords.grade}{Letter grade of state's gun control laws, from Giffords Law Center}
#'   \item{giffords.rank}{Ranking of state's gun control laws, from Giffords Law Center }
#'   \item{gini.2016}{GINI index score}
#'   \item{gini.rank.2016}{Income equality ranking }
#'   \item{govt.worker}{Precentage workforce government workers (2012)}
#'   \item{gun.bgchecks}{Background checks per 100,000 pop (2012)}
#'   \item{gun.dealers}{Gun dealers per 100,000 pop}
#'   \item{gun.deaths.100k}{Gun deaths per 100k}
#'   \item{gun.murders}{Gun murder rate (2010)}
#'   \item{gunlaws}{Number of state gun control laws}
#'   \item{gunlaws.3cat}{Number of state gun control laws, 3 ordinal categories}
#'   \item{gunsammo.rank}{Ranking of best states for gun owners}
#'   \item{hh.income}{Median household income (dollars)}
#'   \item{hispanic.percent}{Percentage of poulation Hispanic or Latino (of any race)}
#'   \item{hispanic.stateleg}{Percent of state legislators who are Hispanic/Latino}
#'   \item{hr.nominate.mean}{Mean NOMINATE score of state's House delegation}
#'   \item{hs.or.more}{Percentage of 25+ population attained at least high school diploma or equivalent}
#'   \item{hs.yrs.ss}{Years of social studies required to graduate high school}
#'   \item{infant.mortality}{Number of infant deaths per 1,000 live births}
#'   \item{judge.selection}{Method used to select appellate court judges}
#'   \item{judges.elected}{Does state elect appellate court judges?}
#'   \item{land.area}{Size of state in square miles}
#'   \item{legalclimate}{State legal climate rating 2015}
#'   \item{legalclimate.rank}{State legal climate ranking 2015}
#'   \item{legis.conservatism}{Rating of conservatism of state legislature}
#'   \item{legis.prof.rank}{State legislative professionalism rank for 2015}
#'   \item{legis.prof.score}{State legislative professionalism score for 2015}
#'   \item{lgbtq.equality.3cat}{Ordinal ranking of state policies for LQBTQ equality}
#'   \item{lgbtq.equality.laws}{Number of laws passed that advance LQBTQ equality}
#'   \item{median.age}{Median age (years)}
#'   \item{medicaid.expansion}{State action on Medicaid expansion pursuant to ACA}
#'   \item{min.wage}{State minimum wage}
#'   \item{obesity.percent}{Percentage of adults with a body mass index of 30.0 or higher}
#'   \item{opioid.rx.rate}{Retail opioid prescriptions dispensed per 100 persons}
#'   \item{over64}{Percentage of population 65 years and over}
#'   \item{polarization.house}{Polarization in State Legislatures, Lower chambers}
#'   \item{polarization.senate}{Polarization in State Legislatures, Upper chambers}
#'   \item{policy.innovation.rate}{Policy adoption rate score}
#'   \item{pop2016}{State population, 2016 (in 100k) }
#'   \item{pop.18.24}{Percentage of population 18 to 24 years old}
#'   \item{population}{State population in 2020}
#'   \item{population.change}{Percentage increase/decrease in population from 2010 to 2020}
#'   \item{pot.policy}{State marijuana laws in 2017}
#'   \item{poverty.rate}{Percentage of people in poverty}
#'   \item{prcapinc}{Per capita income}
#'   \item{preg.teen.rate}{Number of pregnancies per 1,000 women aged 15-19}
#'   \item{preg.uninten.rate}{Unintended pregnancy rate per 1,000 women 15-44}
#'   \item{prochoice.percent}{Percentage of adults who say abortion should be legal in all/most cases}
#'   \item{public.conservative}{Percentage adults self-identifying as conservative}
#'   \item{public.liberal}{Percentage adults self-identifying as liberal}
#'   \item{public.moderate}{Percentage adults self-identifying as moderate}
#'   \item{region}{Census region}
#'   \item{relig.Cath}{Percentage Catholic (2012)}
#'   \item{relig.Prot}{Percentage Protestant (2012)}
#'   \item{relig.high}{Percentage high religiosity (2012)}
#'   \item{relig.import}{Percent religion "A great deal of guidance"}
#'   \item{relig.import.2016}{Overall index of religiosity}
#'   \item{relig.low}{Percentage low religiosity (2012)}
#'   \item{religiosity}{Relig observance-belief scale (Pew)}
#'   \item{religiosity3}{Religiosity}
#'   \item{rtw}{Right to work state?}
#'   \item{schools.avg.salary}{Average salary of public school teachers}
#'   \item{schools.spend}{Expenditure per student in average daily attendance}
#'   \item{schools.st.ratio}{Students enrolled per teacher}
#'   \item{secularism}{Secularism scale (Pew)}
#'   \item{secularism3}{3 quantiles of secularism}
#'   \item{smokers}{Data_Value}
#'   \item{south}{Southern state?}
#'   \item{speak.english.only}{Percentage of population that only speaks English}
#'   \item{state}{State Name}
#'   \item{state.govt.rank}{Overall quality of state government administrative functions}
#'   \item{stateid}{Two-letter abbreviation of state name}
#'   \item{suicide.rate}{Number of deaths due to intentional self-harm per 100,000 population}
#'   \item{tax.source}{State's primary revenue source}
#'   \item{term.limits}{Does state have term limits for legislators?}
#'   \item{trump2016}{Vote share for Trump in 2016 election}
#'   \item{trump2016.ev}{Electoral College votes for Trump in 2016 election}
#'   \item{trump2020}{Two-party vote share for Trump in 2020 election}
#'   \item{trump2020.ev}{Electoral college votes for Trump in 2020 election}
#'   \item{turnout.20vs16}{Difference in voter turnout in 2020 compared to 2016}
#'   \item{under18}{Percentage of population under age 18}
#'   \item{unemployment}{State unemployment rate}
#'   \item{uninsured}{No health insurance coverage}
#'   \item{unionized}{Percent of workers who are union members}
#'   \item{unionized.4cat}{Ordinal-level measurement of state's percentage union membership}
#'   \item{urban}{Percent urban population}
#'   \item{vep16.turnout}{Percent turnout of voting eligible population in 2016}
#'   \item{vep18.turnout}{Percent turnout of voting eligible population in 2018}
#'   \item{vep20.turnout}{Percent turnout of voting eligible population in 2020}
#'   \item{volunteer.hrs.pc}{Volunteer hours per resident}
#'   \item{volunteer.rate}{Volunteer rate}
#'   \item{voter.id.law}{Voter identification law in effect in 2017}
#'   \item{white.percent}{Percentage of population white}
#'   \item{women.stateleg}{Percent of state legislators who are women}
#' }
#' @source Data sources vary.  See Appendix of printed textbook for further information.
"states"


#' World dataset for R Companion to Political Analysis, Third Edition
#'
#' A dataset with variables about countries in the world. This dataset is used to demonstrate application of R to political analysis. See book Appendix for variable names and descriptions.
#'
#' @format A data frame with 169 rows and 206 variables.
#' \describe{
#'   \item{arda.code}{Country numerical code}
#'   \item{bribe.judge}{Prevalence of bribing judges}
#'   \item{bribe.police}{Prevalence of bribing police}
#'   \item{broadband}{Broadband subscription per 100 people}
#'   \item{business.starts}{Number of new corporations registered annually}
#'   \item{cabrv}{Three-letter abbreviation of country name}
#'   \item{carbon.footprint}{National carbon footprint}
#'   \item{ccode}{Numeric country code based on the ISO-3166-1 standard}
#'   \item{ciaedex}{Percent of GDP spent on education}
#'   \item{ciagdpag}{Composition of GDP: Agricultural sector}
#'   \item{ciagdpin}{Composition of GDP: Industrial sector}
#'   \item{ciagdpsv}{Composition of GDP: Service sector}
#'   \item{civil.war}{Civil war intensity}
#'   \item{co2.percap}{Carbon dioxide emissions per capita}
#'   \item{colony}{Colony of what country?}
#'   \item{compulsory.voting}{Does country require citizens to vote?}
#'   \item{confidence}{Confidence in institutions scale}
#'   \item{conflict.index}{Level of violent conflict in country}
#'   \item{conflict.internal}{Number of internal conflict without foreign invention}
#'   \item{conflict.internat}{Number of internal conflict with foreign invention}
#'   \item{corp.tax.rate}{Corporate tax rate}
#'   \item{corrupt.perception}{Corruption perception index}
#'   \item{country}{Country/territory name}
#'   \item{coup.attempts}{Number of attempted coups d'etat since 1950}
#'   \item{coups}{Number of successful coups d'etat since 1950}
#'   \item{covid.cases.permil}{Total COVID cases per million}
#'   \item{covid.deaths.per.million}{Total COVID deaths per million}
#'   \item{covid.response.max}{Maximum of COVID response stringency index}
#'   \item{covid.response.mean}{Mean of COVID response stringency index}
#'   \item{covid.vaccinated}{Percentage of population fully vaccinated against COVID}
#'   \item{death.penalty.status}{Legal status of death penalty}
#'   \item{debt.percent.gdp}{Public debt as a percentage of GDP}
#'   \item{dem.other}{Percentage of other democracies in region}
#'   \item{dem.other5}{Percentage of other democracies in region}
#'   \item{district.size3}{Average number of members per district}
#'   \item{dnpp.3}{Effective number of parliamentary parties}
#'   \item{dpi.cemo}{Is chief executive a military officer?}
#'   \item{dpi.system}{National political system}
#'   \item{durable}{Number of years since the last regime transition}
#'   \item{eco.footprint}{Total ecological footprint}
#'   \item{econ.compete}{Global economic competitiveness}
#'   \item{econ.freedom}{Rating of overall economic freedom}
#'   \item{econ.freedom.5cat}{Rating of overall economic freedom, 5 ordinal categories}
#'   \item{educ.f.avgyrs}{Average Schooling Years, Female}
#'   \item{educ.f.none}{Percentage of Females with No Schooling}
#'   \item{educ.m.avgyrs}{Average Schooling Years, Male}
#'   \item{educ.m.none}{Percentage of Males with No Schooling}
#'   \item{educ.quality}{Average rating of quality of educational system}
#'   \item{effectiveness}{Government effectiveness scale}
#'   \item{eiu.democ.4cat}{Level of democracy, 4 ordinal categories}
#'   \item{eiu.democ.bin}{Is country a democracy?}
#'   \item{eiu.democ.score}{Rating of democracy}
#'   \item{election.integrity}{Integrity of country's electoral system }
#'   \item{election.violence.post}{Were there riots and protest after election?}
#'   \item{election.violence.pre}{Were there riots and protest before election?}
#'   \item{energy.renew.percent}{Percentage of country's energy that is non-fossil fuel }
#'   \item{enpp3.democ}{Effective number of parliamentary parties}
#'   \item{enpp3.democ08}{Effective number of parliamentary parties}
#'   \item{enpp.3}{Effective number of parliamentary parties}
#'   \item{envir.treaty}{Number of environmental treaties agreed to}
#'   \item{eu}{EU member state}
#'   \item{fdi.inflow}{Inflow of foreign direct investment (in millions of US dollars)}
#'   \item{fertility}{Number children born per woman}
#'   \item{fh.democ.3cat}{Rating of democracy, 3 ordinal categories}
#'   \item{fh.democ.score}{Freedom House rating of democracy}
#'   \item{fh.internet.3cat}{Level of Internet freedom in country, 3 ordinal categories}
#'   \item{fh.internet.score}{Measure of Internet freedom}
#'   \item{frac.eth}{Ethnic factionalization}
#'   \item{frac.eth2}{Ethnic factionalization}
#'   \item{frac.eth3}{Ethnic factionalization}
#'   \item{frac.lang}{Language factionalization}
#'   \item{frac.relig}{Religious factionalization}
#'   \item{gas.production}{Gas production (in millions of barrels of oil equivalent)}
#'   \item{gdp.growth}{Annual economic growth rate}
#'   \item{gdp.percap}{Gross domestic product per capita (in U.S. dollars)}
#'   \item{gdp.percap.5cat}{Gross domestic product per person, 5 ordinal categories}
#'   \item{gender.equal3}{Gender empowerment}
#'   \item{gender.inequality}{Index of gender inequality}
#'   \item{gini.index}{GINI index (of income inequality)}
#'   \item{global.social}{Social globalization}
#'   \item{govt.help.cap}{Capacity of state to provide for needy citizens}
#'   \item{govt.integrity}{Rating of government integrity}
#'   \item{govt.per.gdp}{Government spending (all types) as a percentage of GDP}
#'   \item{govt.quality}{The quality of government}
#'   \item{gri}{Index of government restrictions on religion}
#'   \item{grp.name}{Name of government preferred religion}
#'   \item{grp.score}{Index of government religious preference}
#'   \item{happiness}{Average happiness in country}
#'   \item{hdi}{Human development index}
#'   \item{hiv.percent}{Percentage of population aged 15-49 with HIV}
#'   \item{homicide.rate}{Intentional homicides per 100,000 persons}
#'   \item{hospital.beds}{Number of hospital beds per thousand people}
#'   \item{human.flight}{Human flight and brain drain from country}
#'   \item{icc.treaty.ratified}{Has country ratified treaty for International Criminal Court?}
#'   \item{immigrants.percent}{Percentage of population born in another county}
#'   \item{imprisonment.rate}{Number incarcerated per 100,000 persons}
#'   \item{income.tax.rate}{Income tax rate}
#'   \item{indep.judiciary}{Does country have an independent judiciary?}
#'   \item{indy}{Year of independence}
#'   \item{infant.mortality}{Number infants dying before age one per 1,000 live births}
#'   \item{inflation}{Annual inflation rate}
#'   \item{internet.users}{Percentage of population that uses the Internet}
#'   \item{judicial.effectiveness}{Rating of effectiveness of country's judiciary}
#'   \item{judicial.indep.wef}{Average rating of judicial independence}
#'   \item{laws.protect.prop}{Legal protections for private property rights}
#'   \item{legal.origin}{Legal origin of commercial code of country}
#'   \item{legal.quality}{Measure of quality of country's legal institutions}
#'   \item{life.expectancy}{Life expectancy at birth}
#'   \item{lifeex.f}{Life expectancy at birth among females}
#'   \item{lifeex.m}{Life expectancy at birth among males}
#'   \item{literacy}{Literacy rate}
#'   \item{media.access.cand}{Does country provide free or subsidized media access for political candidates?}
#'   \item{media.access.parties}{Does country provide free or subsidized media access for political parties?}
#'   \item{median.age}{Median age in years}
#'   \item{migration.net}{Net migration}
#'   \item{muslim}{Are Muslims predominate religious group?}
#'   \item{ocean.health}{Measure of health of oceans adjacent to country}
#'   \item{oecd}{OECD member state?}
#'   \item{oil}{Oil production, in barrels per day}
#'   \item{oil.production}{Oil production, in metric tons}
#'   \item{organized.crime}{Impact of organized crime on the economy}
#'   \item{peace.5cat}{Peacefulness of country, 5 ordinal categories}
#'   \item{peace.index}{Peacefulness of country}
#'   \item{pmat12.3}{Post-materialism}
#'   \item{pol.terror.scale.ai}{Political terror scale}
#'   \item{pol.terror.scale.hrw}{Political terror scale}
#'   \item{polity.score}{Rating of democracy}
#'   \item{pop.0.14}{Percentage of population age 0-14}
#'   \item{pop.15.64}{Percentage of population age 15-64}
#'   \item{pop.65.older}{Percent of population age 65 and older}
#'   \item{pop.growth}{Percentage population growth/decline annually}
#'   \item{pop.urban}{Percentage of the population living in urban areas}
#'   \item{population}{Size of national population}
#'   \item{population.3cat}{Size of national population, 3 ordinal categories}
#'   \item{population.density}{Number of people per square kilometer}
#'   \item{poverty}{Percentage of the population below the poverty line}
#'   \item{pr.sys}{Proportional representation system?}
#'   \item{press.freedom.fh}{Freedom of the country's press}
#'   \item{press.freedom.rsf}{Freedom of the country's press}
#'   \item{protact3}{Protest activity}
#'   \item{refugees.from}{Refugees from the country who live in other countries}
#'   \item{refugees.impact}{Impact of population displacement on country}
#'   \item{refugees.in}{Refugees from other countries in the country}
#'   \item{regime.type3}{Regime type}
#'   \item{region}{Region name}
#'   \item{regionun}{United Nations region}
#'   \item{religion}{Largest religion by proportion}
#'   \item{reserved.seats}{Does country reserve seats in national legislature for any group?}
#'   \item{rights.assn}{Freedom of assembly and association}
#'   \item{rights.dommov}{Freedom of domestic movement}
#'   \item{rights.formov}{Freedom of foreign movement}
#'   \item{rights.injud}{Independence of the judiciary}
#'   \item{rights.law.index}{Measure of violations of human right and rule of law}
#'   \item{rights.relfree}{Freedom of Religion}
#'   \item{rights.speech}{Freedom of speech}
#'   \item{rights.treaties}{Number of international human rights treaties ratified}
#'   \item{rights.wecon}{Women's economic rights}
#'   \item{rights.wopol}{Women's political rights}
#'   \item{rights.worker}{Worker's rights}
#'   \item{schools.internet}{Average rating of internet availability in schools}
#'   \item{self.employed}{Percentage of labor force that is self-employed}
#'   \item{sexratio}{Sex ratio at birth}
#'   \item{shi}{Social hostility toward religion}
#'   \item{soldiers.percent}{Percentage of labor force in the military}
#'   \item{soldiers.total}{Total number of people in the military}
#'   \item{spendeduc}{Public expenditure on education as a percentage of GDP}
#'   \item{spendhealth}{Public expenditure on health as a percentage of GDP}
#'   \item{spendmil.wdi}{Public expenditure on the military as a percentage of GDP}
#'   \item{tariff.rate}{Tariff rate on imports}
#'   \item{taxes.percent.gdp}{Taxes (all forms) as a percentage of GDP}
#'   \item{terror.index.voh}{Impact of terrorism on the county}
#'   \item{trade.percent.gdp}{International trade as percentage of GDP}
#'   \item{typerel}{Predominant religion}
#'   \item{unemployment}{Percentage of labor force that is unemployed}
#'   \item{unexp.rd}{Public expenditure on research and development as a percentage of GDP}
#'   \item{unfempf}{Ratio of female to male formal employment rates}
#'   \item{unin.inc}{Inequality-adjusted income index}
#'   \item{unineduc}{Inequality-adjusted education index}
#'   \item{unions}{Union density}
#'   \item{unjourn}{Number of verified cases of journalists imprisoned}
#'   \item{unlit}{Adult literacy rate }
#'   \item{unmobcov}{Percentage covered by a mobile phone network}
#'   \item{unmort.f}{Number of adult female deaths per 1,000 females}
#'   \item{unmort.m}{Number of adult male deaths per 1,000 males}
#'   \item{unnewsp}{Daily newspapers per thousand people}
#'   \item{unnoncom}{Death rates from non-communicable diseases}
#'   \item{unpop30}{Projected 2030 population in millions}
#'   \item{unremitp}{Per capita remittance inflows in US dollars}
#'   \item{unremitt}{Remittance inflows as a percentage of GDP}
#'   \item{unsathlt}{Percentage satisfied with their personal health}
#'   \item{unsati}{Overall life satisfaction}
#'   \item{unsatif}{Overall life satisfaction among females}
#'   \item{unsatjob}{Percentage satisfied with their job}
#'   \item{unsatliv}{Percentage satisfied with their standard of living}
#'   \item{unseced}{Percentage with at least secondary education}
#'   \item{vdem.2cat}{Is country a democracy or autocracy?}
#'   \item{vdem.4cat}{Ordinal ranking of democracy, 4 categories}
#'   \item{vdem.edi.score}{Electoral democracy index}
#'   \item{vdem.ldi.score}{Liberall democracy index}
#'   \item{vi.rel3}{Percent saying religion very important}
#'   \item{violence.cost}{Economic cost of violence on national economy}
#'   \item{votevap10s}{Turnout of voting age population in 2010s }
#'   \item{womenleg}{Percent women in lower house of legislature}
#'   \item{womyear}{Year women first enfranchised}
#'   \item{womyear2}{Year women first enfranchised}
#'   \item{youngleg}{Percentage of lower house of legislature aged 40 years or younger}
#' }
#' @source Sources vary.  See Appendix of printed textbook for further information.
"world"




#' NES dataset for R Companion to Political Analysis, Third Edition
#'
#' The American National Election Survey polls individuals about their political beliefs and behavior. This dataset is used to demonstrate application of R to political analysis. See book Appendix for variable names and descriptions.
#'
#' @format A data frame with 8280 rows and 429 variables.
#' \describe{
#'   \item{abortion.imp}{PRE: Importance of abortion issue to R}
#'   \item{abortion.legal}{PRE: STD Abortion: self-placement}
#'   \item{abortion.scotus}{PRE: SUMMARY: Abortion rights Supreme Court}
#'   \item{active.duty.mil}{PRE: Armed forces active duty}
#'   \item{address.yrs}{PRE: Years R lived at address}
#'   \item{age}{PRE: SUMMARY: Respondent age}
#'   \item{agree.facts}{PRE: How important that people agree on basic facts}
#'   \item{allow.refugees}{POST: SUMMARY: Favor/oppose allowing refugees to come to US}
#'   \item{american.id.import}{POST: How important is being American to R's identity}
#'   \item{angry.about.things}{PRE: How angry R feels about how things are going in the country}
#'   \item{approve.aca}{POST: SUMMARY: Approve/disapprove Affordable Care Act}
#'   \item{approve.cong}{PRE: SUMMARY: Approval of Congress handling its job}
#'   \item{approve.gov.covid}{PRE: SUMMARY: Approve or disapprove R's governor handling COVID-19}
#'   \item{approve.local.covid}{PRE: SUMMARY: Approve or disapprove local government handling COVID-19}
#'   \item{approve.pres.covid}{PRE: SUMMARY: Approve or disapprove President handling COVID-19}
#'   \item{approve.pres.econ}{PRE: SUMMARY: Approve or disapprove President handling economy}
#'   \item{approve.pres.hc}{PRE: SUMMARY: Approve or disapprove President handling health care}
#'   \item{approve.pres.imm}{PRE: SUMMARY: Approve or dissaprove President handling immigration}
#'   \item{approve.pres.ir}{PRE: SUMMARY: Approve or disapprove President handling foreign relations}
#'   \item{approve.pres.job}{PRE: SUMMARY: Approve or disapprove President handling job}
#'   \item{ban.assault.rif}{POST: SUMMARY: Favor/oppose banning 'assault-style' rifles}
#'   \item{been.arrested}{POST: Has R ever been arrested}
#'   \item{bible.god.man}{PRE: Is Bible word of God or men}
#'   \item{biden.cares}{PRE: Democratic Presidential candidate trait: really cares}
#'   \item{biden.honest}{PRE: Democratic Presidential candidate trait: honest}
#'   \item{biden.knowledge}{PRE: Democratic Presidential candidate trait: knowledgeable}
#'   \item{biden.libcon7}{PRE: 7pt scale liberal-conservative: Democratic Presidential candidate}
#'   \item{biden.strlead}{PRE: Democratic Presidential candidate trait: strong leadership}
#'   \item{birthright.citizens}{PRE: SUMMARY: Favor or oppose ending birthright citizenship}
#'   \item{blacks.gotless}{POST: Agree/disagree: blacks have gotten less than they deserve}
#'   \item{blacks.pastdiff}{POST: Agree/disagree: past slavery & discrimination make it difficult for blacks}
#'   \item{blacks.tryharder}{POST: Agree/disagree: if blacks tried harder they'd be as well off as whites}
#'   \item{blacks.workforit}{POST: Agree/disagree: blacks should work their way up without special favors}
#'   \item{border.wall}{PRE: SUMMARY: Favor or oppose building a wall on border with Mexico}
#'   \item{born.in.usa}{PRE: Rs: born US, Puerto Rico, or some other country}
#'   \item{buy.back.rifles}{POST: Favor/oppose government buy back of 'assault-style' rifles}
#'   \item{campaign.news.carlson}{PRE: Mention: TV PROG - Tucker Carlson Tonight (Fox)}
#'   \item{campaign.news.colbert}{PRE: Mention: TV PROG - The Late Show with Stephen Colbert}
#'   \item{campaign.news.hannity}{PRE: Mention: TV PROG - Hannity (Fox)}
#'   \item{campaign.news.maddow}{PRE: Mention: TV PROG - The Rachel Maddow Show (MSNBC)}
#'   \item{campaign.news.none}{PRE: Media sources R used to follow presidential campaign: none}
#'   \item{campaign.news.papers}{PRE: Media sources R used to follow presidential campaign: newspapers}
#'   \item{campaign.news.radio}{PRE: Media sources R used to follow presidential campaign: radio news}
#'   \item{campaign.news.tv}{PRE: Media sources R used to follow presidential campaign: tv programs}
#'   \item{campaign.news.web}{PRE: Media sources R used to follow presidential campaign: internet sites}
#'   \item{campaign.spendlim}{POST: Limits on campaign spending}
#'   \item{campaigns.interest}{PRE: How interested in following campaigns}
#'   \item{cant.get.ahead}{POST: Because of rich and powerful it's difficult for the rest to get ahead}
#'   \item{care.who.wins}{PRE: How much R cares who wins Presidential Election [revised]}
#'   \item{case.id}{2020 Case ID}
#'   \item{censor.self}{PRE: How often self censor}
#'   \item{changed.names}{PRE: R name ever changed}
#'   \item{child.behave}{POST: Which child trait more important: considerate or well-behaved}
#'   \item{child.manners}{POST: Which child trait more important: curiosity or good manners}
#'   \item{child.obey}{POST: Which child trait more important: obedience or self-reliance}
#'   \item{child.respect}{POST: Which child trait more important: independence or respect}
#'   \item{citizenship.path}{POST: SUMMARY: Favor/oppose providing path to citizeship}
#'   \item{civ12.argue}{POST: Has R in past 12 months: gotten into a political argument}
#'   \item{civ12.comment}{POST: Has R in past 12 months: posted comment online about political issue}
#'   \item{civ12.community}{POST: Has R in past 12 months: worked w/others to deal w/issue facing community}
#'   \item{civ12.cong}{POST: Has R in past 12 months: contacted member of US Senate or House of Rep}
#'   \item{civ12.fedoff}{POST: Has R in past 12 months: contacted non-elected official in federal govt}
#'   \item{civ12.fedpol}{POST: Has R in past 12 months: contacted federal elected official}
#'   \item{civ12.giveorg}{POST: Has R in past 12 months: given money to other organization}
#'   \item{civ12.giverelig}{POST: Has R in past 12 months: given money to religious organization}
#'   \item{civ12.march}{POST: Has R in past 12 months: joined a protest march, rally, or demonstration}
#'   \item{civ12.meeting}{POST: Has R in past 12 months: attend mtg about issue facing community/schools}
#'   \item{civ12.petition}{POST: Has R in past 12 months: sign internet or paper petition}
#'   \item{civ12.stateoff}{POST: Has R in past 12 months: contacted non-elected official in state/local gov}
#'   \item{civ12.statepol}{POST: Has R in past 12 months: contacted elected official on state/local level}
#'   \item{civ12.vol}{POST: Has R in past 12 months: done any volunteer work}
#'   \item{climate.ch.weather}{POST: How much is climate change affecting severe weather/temperatures in US}
#'   \item{climate.import}{POST: How important is issue of climate change to R}
#'   \item{community.yrs}{PRE: How long lived in this community YRS}
#'   \item{consumer.politics}{POST: How often bought or boycotted product/service for social/political reasons}
#'   \item{contacted.gotv}{POST: Anyone talk to R about registering or getting out to vote}
#'   \item{covid.election}{PRE: Options for election if COVID-19 continues}
#'   \item{covid.fed}{PRE: SUMMARY: Federal government response to COVID-19}
#'   \item{covid.made.lab}{POST: Was the coronavirus (COVID-19) was developed intentionally in a lab or not}
#'   \item{covid.reopening}{PRE: SUMMARY: Re-opening too quickly or too slowly}
#'   \item{covid.restrictions}{PRE: Limits placed on public activity due to COVID-19 too strict or not}
#'   \item{covid.science.help}{POST: How important should science be for decisions about COVID-19}
#'   \item{death.penalty}{PRE: SUMMARY: R favor/oppose death penalty}
#'   \item{def.spend.7pt}{PRE: 7pt scale defense spending: self-placement}
#'   \item{deficit.reduce}{POST: Importance of reducing deficit}
#'   \item{dem.libcon7}{PRE: 7pt scale liberal-conservative: Democratic party}
#'   \item{deport.children}{PRE: SUMMARY: Should children brought illegally be sent back or allowed to stay}
#'   \item{deport.unauth}{POST: SUMMARY: Favor/oppose returning unauthorize immigrants to native country}
#'   \item{discrim.vs.asians}{POST: Discrimination in the US against Asians}
#'   \item{discrim.vs.blacks}{POST: Discrimination in the US against blacks}
#'   \item{discrim.vs.christians}{POST: Discrimination in the US against Christians}
#'   \item{discrim.vs.glb}{POST: Discrimination in the US against Gays and Lesbians}
#'   \item{discrim.vs.hispanics}{POST: Discrimination in the US against Hispanics}
#'   \item{discrim.vs.men}{POST: Discrimination in the US against men}
#'   \item{discrim.vs.muslims}{POST: Discrimination in the US against Muslims}
#'   \item{discrim.vs.trans}{POST: Discrimination in the US against transgender people}
#'   \item{discrim.vs.whites}{POST: Discrimination in the US against whites}
#'   \item{discrim.vs.women}{POST: Discrimination in the US against women}
#'   \item{diversity.good.usa}{POST: SUMMARY: Increasing diversity made US better/worse place to live}
#'   \item{divided.govt}{PRE: Party Control or split government}
#'   \item{donations.change.votes}{POST: Congress change votes because of donation to campaign}
#'   \item{econ.current}{PRE: Current economy good or bad}
#'   \item{econ.lastyear}{PRE: SUMMARY: National economy better or worse in last year}
#'   \item{econ.mobility.now}{POST: SUMMARY: Economic mobility}
#'   \item{econ.nextyear}{PRE: SUMMARY: Economy better or worse in next 12 months}
#'   \item{econ.worry}{PRE: How worried about national economy}
#'   \item{educ.5cat}{PRE: SUMMARY: Respondent 5 Category level of education}
#'   \item{education}{PRE: Highest level of Education}
#'   \item{elect.asians}{POST: How important that more Asians get elected to political office}
#'   \item{elect.blacks}{POST: How important that more blacks get elected to political office}
#'   \item{elect.hispanics}{POST: How important that more Hispanics get elected to political office}
#'   \item{elect.lgbt}{POST: How important that more LGBT people get elected to political office}
#'   \item{elect.women}{POST: How important that more women get elected to political office}
#'   \item{elections.govt.attn}{PRE: Elections make government pay attention}
#'   \item{envir.or.biz}{PRE: 7pt scale environment-business tradeoff: self-placement}
#'   \item{equal.opp}{POST: Society should make sure everyone has equal opportunity}
#'   \item{facebook.polpost}{POST: How often post political content on Facebook}
#'   \item{facebook.use}{POST: How often use Facebook}
#'   \item{faced.gender.discrim}{POST: How much discrimination has R faced because of gender}
#'   \item{faced.race.discrim}{POST: How much discrimination has R faced personally because or race/ethnicity}
#'   \item{fed.bw.better}{POST: SUMMARY: Federal government treats blacks or whites better}
#'   \item{fedspend.aidpoor}{PRE: SUMMARY: Federal Budget Spending: aid to the poor}
#'   \item{fedspend.border}{PRE: SUMMARY: Federal Budget Spending: Tightening border security}
#'   \item{fedspend.crime}{PRE: SUMMARY: Federal Budget Spending: dealing with crime}
#'   \item{fedspend.environ}{PRE: SUMMARY: Federal Budget Spending: protecting the environment}
#'   \item{fedspend.highways}{PRE: SUMMARY: Federal Budget Spending: building and repairing highways}
#'   \item{fedspend.schools}{PRE: SUMMARY: Federal Budget Spending: public schools}
#'   \item{fedspend.socsec}{PRE: SUMMARY: Federal Budget Spending: Social Security}
#'   \item{fedspend.welfare}{PRE: SUMMARY: Federal Budget Spending: welfare programs}
#'   \item{feminist}{POST: Does R consider themself a feminist or anti-feminist}
#'   \item{feminist.import}{POST: How important is being a feminist}
#'   \item{finance.lastyear}{PRE: R how much better or worse off financially than 1 year ago}
#'   \item{finance.nextyear}{PRE: R how much better or worse off financially next year}
#'   \item{financial.worried}{PRE: How worried is R about current financial situation}
#'   \item{ft.asian.am}{POST: Feeling thermometer: Asian-Americans}
#'   \item{ft.asians}{POST: Feeling thermometer: Asians}
#'   \item{ft.biden.post}{POST: Feeling thermometer: Democratic Presidential candidate: Joe Biden}
#'   \item{ft.biden.pre}{PRE: Feeling Thermometer: Joe Biden, Democratic Presidential candidate}
#'   \item{ft.bigbiz}{POST: Feeling thermometer: big business}
#'   \item{ft.blacks}{POST: Feeling thermometer: blacks}
#'   \item{ft.blm}{POST: Feeling thermometer: Black Lives Matter}
#'   \item{ft.capitalists}{POST: Feeling thermometer: capitalists}
#'   \item{ft.cdc}{POST: Feeling thermometer: Center for Disease Control (CDC)}
#'   \item{ft.christian.fund}{POST: Feeling thermometer: Christian fundamentalists}
#'   \item{ft.christians}{POST: Feeling thermometer: Christians}
#'   \item{ft.congress}{POST: Feeling thermometer: congress}
#'   \item{ft.conservatives}{POST: Feeling thermometer: conservatives}
#'   \item{ft.dem}{PRE: Feeling Thermometer: Democratic Party}
#'   \item{ft.fauci}{POST: Feeling thermometer: Dr. Anthony Fauci}
#'   \item{ft.fbi}{POST: Feeling thermometer: Federal Bureau of Investigation (FBI)}
#'   \item{ft.feminists}{POST: Feeling thermometer: feminists}
#'   \item{ft.gays.lesbians}{POST: Feeling thermometer: gay men and lesbians}
#'   \item{ft.harris.post}{POST: Feeling thermometer: Democratic Vice Presidential candidate: Kamala Harris}
#'   \item{ft.harris.pre}{PRE: Feeling Thermometer: Kamala Harris, Democratic Vice-Presidential candidate}
#'   \item{ft.hispanics}{POST: Feeling thermometer: Hispanics}
#'   \item{ft.ice}{POST: Feeling thermometer: Immigration and Customs Enforcement (ICE) agency}
#'   \item{ft.illegal.imm}{POST: Feeling thermometer: illegal immigrants}
#'   \item{ft.jews}{POST: Feeling thermometer: Jews}
#'   \item{ft.journalists}{POST: Feeling thermometer: journalists}
#'   \item{ft.liberals}{POST: Feeling thermometer: liberals}
#'   \item{ft.metoo}{POST: Feeling thermometer: #MeToo movement}
#'   \item{ft.muslims}{POST: Feeling thermometer: Muslims}
#'   \item{ft.nato}{POST: Feeling thermometer: North Atlantic Treaty Organization (NATO)}
#'   \item{ft.nra}{POST: Feeling thermometer: National Rifle Association (NRA)}
#'   \item{ft.obama}{PRE: Feeling Thermometer: Barack Obama}
#'   \item{ft.pence.post}{POST: Feeling thermometer: Republican Vice Presidential candidate: Mike Pence}
#'   \item{ft.pence.pre}{PRE: Feeling Thermometer: Mike Pence, Republican Vice-Presidential candidate}
#'   \item{ft.police}{POST: Feeling thermometer: police}
#'   \item{ft.pp}{POST: Feeling thermometer: Planned Parenthood}
#'   \item{ft.rep}{PRE: Feeling Thermometer: Republican Party}
#'   \item{ft.rural}{POST: Feeling thermometer: rural Americans}
#'   \item{ft.scientists}{POST: Feeling thermometer: scientists}
#'   \item{ft.scotus}{POST: Feeling thermometer: U.S. Supreme Court}
#'   \item{ft.socialists}{POST: Feeling thermometer: socialists}
#'   \item{ft.transgender}{POST: Feeling thermometer: transgender people}
#'   \item{ft.trump.post}{POST: Feeling thermometer: Republican Presidential candidate: Donald Trump}
#'   \item{ft.trump.pre}{PRE: Feeling Thermometer: Donald Trump, Republican Presidential candidate}
#'   \item{ft.un}{POST: Feeling thermometer: United Nations (UN)}
#'   \item{ft.unions}{POST: Feeling thermometer: labor unions}
#'   \item{ft.whites}{POST: Feeling thermometer: whites}
#'   \item{ft.who}{POST: Feeling thermometer: World Health Organization (WHO)}
#'   \item{gay.adopt}{PRE: Should gay and lesbian couples be allowed to adopt}
#'   \item{gay.job.discrim}{PRE: SUMMARY: Favor/oppose laws protect gays lesbians against job discrimination}
#'   \item{gay.marriage}{PRE: R position on gay marriage}
#'   \item{gay.req.service}{PRE: SUMMARY: Services to same sex couples}
#'   \item{gender}{PRE: What is your (R) sex? [revised]}
#'   \item{gov.asst.blacks}{PRE: 7pt scale gov assistance to blacks scale: self-placement}
#'   \item{govt.act.ineq}{POST: SUMMARY: Favor/oppose government trying to reduce income inequality}
#'   \item{govt.act.warm.str}{PRE: Government action about rising temperatures (STRENGTH)}
#'   \item{govt.act.warming}{PRE: Government action about rising temperatures}
#'   \item{govt.corrupt}{PRE: How many in government are corrupt}
#'   \item{govt.guar.job}{PRE: 7pt scale guaranteed job-income scale: self-placement}
#'   \item{govt.hc.7pt}{PRE: 7pt scale gov-private medical insurance scale: self-placement}
#'   \item{govt.help.hc}{POST: SUMMARY: Increase/decrease government spending to help pay for health care}
#'   \item{govt.run.byfew}{PRE: Government run by a few big interests or for benefit of all}
#'   \item{govt.services.7pt}{PRE: 7pt scale spending & services: self-placement}
#'   \item{govt.under.media}{PRE: How concerned government might undermine media}
#'   \item{govt.wastes.money}{PRE: Does government waste much tax money}
#'   \item{govtreg.moreless}{POST: Would it be good for society to have more or less government regulation}
#'   \item{grand.born.usa}{PRE: How many grandparents born outside the US}
#'   \item{grew.up.where}{PRE: Where R grew up}
#'   \item{gun.bg.checks}{POST: SUMMARY: Favor/oppose background checks for gun puchases}
#'   \item{gun.buying}{POST: Should federal government make it more difficult or easier to buy a gun}
#'   \item{gun.issue.imp}{POST: How important is issue of gun access to R}
#'   \item{guns.owned}{PRE: How many Guns owned}
#'   \item{happy.about.things}{PRE: How happy R feels about how things are going in the country}
#'   \item{hardvote2020}{POST: How difficult was it for R to vote}
#'   \item{harrassed.work}{POST: Has R experienced harrassment at work}
#'   \item{harrassed.work.oft}{POST: How often has R experienced harrassment at work}
#'   \item{has.daughter}{POST: Does R have any sons or daughters - one or more daughters}
#'   \item{has.nokids}{POST: Does R have any sons or daughters - no sons and no daughters}
#'   \item{has.son}{POST: Does R have any sons or daughters - one or more sons}
#'   \item{have.health.ins}{PRE: Does R have health insurance}
#'   \item{hc.pay.bills}{PRE: How likely R able to pay all health care costs in next 12 months}
#'   \item{health}{PRE: Health of R}
#'   \item{health.lose.ins}{PRE: R concerned about losing health insurance}
#'   \item{health.pay.costs}{PRE: R concerned about paying for health care}
#'   \item{help.with.science}{POST: How much do people need help from experts to understand science}
#'   \item{hh.covid.symp}{PRE: Anyone in household COVID-19 based on symptoms}
#'   \item{hh.covid.test}{PRE: Anyone in household tested pos for COVID-19}
#'   \item{hh.family.mem}{PRE: R living with how many family members}
#'   \item{hh.income}{PRE-POST: SUMMARY: Total (family) income}
#'   \item{hh.income.pre}{PRE: SUMMARY: Total (family) income}
#'   \item{hh.landline}{PRE: Is there a working HH landline phone}
#'   \item{hh.num.child}{PRE: How many children in HH age 0-17}
#'   \item{hh.partner.status}{PRE: Domestic partnership status}
#'   \item{hh.union.mem}{PRE: Anyone in HH belong to labor union}
#'   \item{housing.payments}{PRE: How likely R able to make all housing payments in next 12 months}
#'   \item{hydrox.treat.covid}{POST: Evidence that hydroxychloroquine is effective treatment for COVID-19 or no}
#'   \item{immig.crime}{POST: SUMMARY: effect of illegal immiration on crime rate}
#'   \item{immig.levels}{POST: What should immigration levels be}
#'   \item{immig.policy}{PRE: US government policy toward unauthorized immigrants}
#'   \item{immig.take.jobs}{POST: How likely immigration will take away jobs}
#'   \item{imp.govt.checks}{PRE: How important branches of government keep one another from too much power}
#'   \item{imp.media.crit}{PRE: How important that news organizations free to criticize}
#'   \item{imp.off.conseq}{PRE: How important elected officials face serious consequences for misconduct}
#'   \item{imports.limit}{POST: SUMMARY: Favor/oppose new limits on imports}
#'   \item{income.gap.change}{PRE: SUMMARY: How much larger is income gap today}
#'   \item{income.gap.today}{PRE: Income gap today more or less than 20 years ago}
#'   \item{ineq.worryless}{POST: We'd be better off if worried less about equality}
#'   \item{intl.force}{PRE: Force to solve international problems}
#'   \item{invest.stocks}{PRE: Money invested in Stock Market}
#'   \item{laws.contrib.indiv}{POST: Congress pass laws that benefit contributor individuals}
#'   \item{laws.contrib.org}{POST: Congress pass laws that benefit contributor organization}
#'   \item{leader.compromise}{PRE: Prefer government official who compromises or sticks to principles}
#'   \item{libcon3}{PRE: 3pt scale liberal-conservative self-placement}
#'   \item{libcon7}{PRE: 7pt scale liberal-conservative self-placement}
#'   \item{life.sat}{PRE: How satisfied is R with life}
#'   \item{lifex.buyusa}{POST: Life experience: does R choose products because they are made in America}
#'   \item{lifex.flyflag}{POST: Life experience: has R displayed American flag on house in past year}
#'   \item{lifex.foodstamps}{POST: Life experience: has R ever received food stamps or other public assistanc}
#'   \item{lifex.huntfish}{POST: Life experience: has R gone hunting or fishing in past year}
#'   \item{lifex.knowimmig}{POST: Life experience: does R know someone moved to U.S. from another country}
#'   \item{lifex.oweloans}{POST: Life experience: does R currently owe money on student loans}
#'   \item{lifex.retireacct}{POST: Life experience: does R a have pension or retirement account}
#'   \item{lifex.ridebus}{POST: Life experience: has R used public transportation in past year}
#'   \item{lifex.sharkbite}{POST: Life experience: has R ever been bitten by a shark}
#'   \item{lqb.friendfam}{POST: R has family/neighbors/coworkers/friends who are gay, lesbian or bisexual}
#'   \item{marital}{PRE: Marital status}
#'   \item{medical.putoff}{PRE: Put off checkup and vaccines}
#'   \item{metoo.toofar}{POST: SUMMARY: Attention to sexual harrassment as gone too far/not far enough}
#'   \item{middle.class.ext}{POST: Is R lower middle class, middle class, upper middle class? [EGSS]}
#'   \item{min.wage.change}{POST: Should the minimum wage be raised, kept the same, or lowered}
#'   \item{morals.adjust}{POST: The world is changing & we should adjust view of moral behavior}
#'   \item{morechance.okay}{POST: Not a big problem if some have more chance in life}
#'   \item{moreless.govt}{POST:SUMMARY: Less or more government}
#'   \item{opioid.addiction}{POST: SUMMARY Should federal govt do more/less about opioid drug addiction}
#'   \item{oth.race.concern}{POST: How often does R have concerned feelings for other racial/ethnic groups}
#'   \item{oth.race.feel}{POST: How often R imagines how they would feel before criticizing other groups}
#'   \item{oth.race.persp}{POST: How often does R try to understand perpective of other racial/ethnic group}
#'   \item{oth.race.protect}{POST: How often R feels protective of someone due to race or ethnicity}
#'   \item{paid.parent.leave}{PRE: SUMMARY: Require employers to offer paid leave to parents of new children}
#'   \item{parents.born.usa}{PRE: Native status of parents}
#'   \item{party.register}{PRE-POST: SUMMARY: Party of registration}
#'   \item{partyid3}{PRE: Party ID, 3 categories}
#'   \item{partyid7}{PRE: SUMMARY: Party ID}
#'   \item{partyid.importance}{PRE: Party identity importance}
#'   \item{people.fed.lies}{POST: Much of what people hear in schools and media are lies by those in power}
#'   \item{people.too.sens}{PRE: Need to be more sensitive talking or people too easily offended}
#'   \item{person.get.ahead}{POST: How much opportunity in America for average person to get ahead}
#'   \item{pol.asian.infl}{POST: How much influence do Asians have in US politics}
#'   \item{pol.black.infl}{POST: How much influence do blacks have in US politics}
#'   \item{pol.dontcare}{POST: [STD] Public officials don't care what people think}
#'   \item{pol.for.insiders}{POST: Our political system only works for insiders with money and power}
#'   \item{pol.hispanic.infl}{POST: How much influence do Hispanics have in US politics}
#'   \item{pol.hurt.fam}{POST: How much have political differences hurt relationships w/family}
#'   \item{pol.nosay}{POST: [STD] Have no say about what goverment does}
#'   \item{pol.oligarchy}{POST: Business and politics controlled by few powerful people}
#'   \item{pol.toocomplex}{POST: [REV] Politics/government too complicated to understand}
#'   \item{pol.understand}{POST: [REV] How well does R understand important political issues}
#'   \item{pol.white.infl}{POST: How much influence do whites have in US politics}
#'   \item{polact.givecand}{POST: R contribute money to individual candidate running for public office}
#'   \item{polact.giveoth}{POST: R contribute to any other group that supported or opposed candidates}
#'   \item{polact.giveparty}{POST: R contribute money to political party during this election year}
#'   \item{polact.meetings}{POST: R go to any political meetings, rallies, speeches, dinners}
#'   \item{polact.onlinemeet}{POST: R attend online political meetings, rallies, speeches, fundraisers}
#'   \item{polact.othwork}{POST: R do any (other) work for party or candidate}
#'   \item{polact.postsign}{POST: R wear campaign button or post sign or bumper sticker}
#'   \item{polact.talkpol}{POST: R ever discuss politics with family or friends}
#'   \item{polact.talkvote}{POST: R talk to anyone about voting for or against a party or candidate}
#'   \item{police.bw.better}{POST: SUMMARY: Police treat blacks or whites better}
#'   \item{police.stop.lastyr}{POST: During past 12 months, R  or any family members stopped by police}
#'   \item{police.useforce}{POST: How often do police officers use more force than necessary}
#'   \item{political.violence}{PRE: SUMMARY: Political violence compared to 4 yrs ago}
#'   \item{politics.attention}{PRE: How often does R pay attention to politics and elections}
#'   \item{polquiz.fedspend}{PRE: On which program does Federal government spend the least}
#'   \item{polquiz.german}{POST: Office recall: German Chancellor - Angela Merkel [coded/scheme 1]}
#'   \item{polquiz.housemaj}{PRE: Party with most members in House before election}
#'   \item{polquiz.russian}{POST: Office recall: Russian President - Vladimir Putin [coded/scheme 1]}
#'   \item{polquiz.scotus}{POST: Office recall: SCOTUS Chief Justice - John Roberts [coded/scheme 1]}
#'   \item{polquiz.sen.term}{PRE: How many years in full term for US Senator}
#'   \item{polquiz.senatemaj}{PRE: Party with most members in Senate before election}
#'   \item{polquiz.speaker}{POST: Office recall: Speaker of the House - Nancy Pelosi [coded/scheme 1]}
#'   \item{polquiz.vp}{POST: Office recall: Vice-President - Mike Pence [coded]}
#'   \item{postmat.1a}{POST: Post materialism most important 1A}
#'   \item{postmat.1b}{POST: Post materialism next most important 1B}
#'   \item{postmat.2a}{POST: Post materialism most important 2A}
#'   \item{postmat.2b}{POST: Post materialism next most important 2B}
#'   \item{pref.hiring.blacks}{POST: SUMMARY: Favor/oppose preferential hiring/promotion of blacks}
#'   \item{pres.ask.foreign}{PRE: Appropriate/inappropriate Pres ask foreign countries to investigate rivals}
#'   \item{pres.nochecks}{PRE: SUMMARY: Helpful/harmful if Pres didn't have to worry about congress/courts}
#'   \item{presvote2020}{PRE-POST: SUMMARY: 2020 Presidential vote}
#'   \item{primary.voter}{PRE: Did R vote in a Presidential primary or caucus}
#'   \item{protestors.conduct}{PRE: SUMMARY: Protestors actions been mostly violent or peaceful}
#'   \item{race.ethnicity}{PRE: SUMMARY: R self-identified race/ethnicity}
#'   \item{reddit.polpost}{POST: How often post political content on Reddit}
#'   \item{reddit.use}{POST: How often use Reddit}
#'   \item{reg.greenhouse}{POST: SUMMARY: Favor/oppose increased regulation on greenhouse emissions}
#'   \item{region}{SAMPLE: Census region}
#'   \item{relig.ever.attend}{PRE: Ever attend church or religious services}
#'   \item{relig.how.often}{PRE: Attend religious services how often}
#'   \item{religion}{PRE: What is present religion of R}
#'   \item{religion.group}{PRE: SUMMARY: Major group religion summary}
#'   \item{religion.imp}{PRE: Is religion important part of R life [revised]}
#'   \item{religious.id}{PRE: Religious identification}
#'   \item{rep.libcon7}{PRE: 7pt scale liberal-conservative: Republican party}
#'   \item{restrict.journalists}{PRE: SUMMARY: Favor or oppose restricting journalist access}
#'   \item{rural.getmore}{POST: SUMMARY: People in rural areas get more/less from government}
#'   \item{rural.influence}{POST: SUMMARY: People in rural areas have too much/too little influence}
#'   \item{rural.respect}{POST: SUMMARY: People in rural areas get too much/too little respect}
#'   \item{rural.urban}{POST: Does R currently live in a rural or urban area}
#'   \item{rural.urban.id}{POST: How important is urban or rural to R's identity}
#'   \item{russia.int.election}{PRE: Likelihood of Russian interference in upcoming election}
#'   \item{russia.interfere}{POST: Did Russia try to interfere in 2016 presidential election or not}
#'   \item{separate.children}{POST: SUMMARY: Favor/oppose separating children of detained immigrants}
#'   \item{sexual.orient}{PRE: Sexual orientation of R [revised]}
#'   \item{smoke.cig.life}{POST: R smoked 100 cigarettes in life}
#'   \item{smoke.cig.now}{POST: R currently smoking}
#'   \item{social.class}{POST: How would R describe social class [EGSS]}
#'   \item{speak.english}{PRE: How important to speak English in US}
#'   \item{split.ticket}{PRE: Split-ticket voting}
#'   \item{state}{SAMPLE: Sample location FIPS state}
#'   \item{stateabbr}{SAMPLE: Sample location state postal abbreviation}
#'   \item{survey.serious}{PRE: How often took survey seriously}
#'   \item{talkpol.week}{POST: How many days in past week discussed politics with family or friends}
#'   \item{tax.rich}{POST: Favor or oppose tax on millionaires}
#'   \item{terrorism.worry}{POST: DHS: How worried about terrorist attack in near future}
#'   \item{threat.from.china}{POST: How much is China a threat to the United States}
#'   \item{threat.from.germany}{POST: How much is Germany a threat to the United States}
#'   \item{threat.from.iran}{POST: How much is Iran a threat to the United States}
#'   \item{threat.from.japan}{POST: How much is Japan a threat to the United States}
#'   \item{threat.from.mexico}{POST: How much is Mexico a threat to the United States}
#'   \item{threat.from.russia}{POST: How much is Russia a threat to the United States}
#'   \item{trad.fam.values}{POST: Fewer problems if there was more emphasis on traditional family values}
#'   \item{trade.agreements}{POST: SUMMARY: Favor/oppose free trade agreements}
#'   \item{trade.good.ir}{POST: SUMMARY: Increasing trade good/bad for international relationships}
#'   \item{trade.jobs.abroad}{POST: SUMMARY: International trade increaded/decreased jobs abroad}
#'   \item{trade.jobs.usa}{POST: SUMMARY: International trade increased/decreased jobs in US}
#'   \item{trans.friendfam}{POST: R has family/neighbors/coworkers/friends who are transgender}
#'   \item{trans.military}{POST: SUMMARY: Favor/oppose transender people serve in military}
#'   \item{trans.policy}{PRE: SUMMARY: Transgender policy}
#'   \item{treat.people.fair}{POST: If people were treated more fairly we would have fewer problems}
#'   \item{trump.acquittal}{PRE: SUMMARY: Favor or oppose Senate acquittal decision}
#'   \item{trump.cares}{PRE: Republican Presidential Candidate trait: really cares}
#'   \item{trump.corruption}{PRE: SUMMARY: Corruption increased or decreased since Trump}
#'   \item{trump.deport.more}{POST: Did Trump administration deport more immigrants or did Obama}
#'   \item{trump.honest}{PRE: Republican Presidential Candidate trait: honest}
#'   \item{trump.impeachment}{PRE: SUMMARY: Favor or oppose House impeachment decision}
#'   \item{trump.knowledge}{PRE: Republican Presidential Candidate trait: knowledgeable}
#'   \item{trump.libcon7}{PRE: 7pt scale liberal-conservative: Republican Presidential candidate}
#'   \item{trump.strlead}{PRE: Republican Presidential Candidate trait: strong leadership}
#'   \item{trump.ukraine}{PRE: Did Trump ask Ukraine to investigate rivals}
#'   \item{trust.dc}{PRE: How often trust government in Washington to do what is right [revised]}
#'   \item{trust.election.off}{PRE: Trust election officials}
#'   \item{trust.experts}{POST: SUMMARY: Trust ordinary people/experts for public policy}
#'   \item{trust.media}{PRE: How much trust in news media}
#'   \item{trust.people}{PRE: How often can people be trusted}
#'   \item{turnout2020}{PRE-POST: SUMMARY: Voter turnout in 2020}
#'   \item{twitter.polpost}{POST: How often post political content on Twitter}
#'   \item{twitter.use}{POST: How often use Twitter}
#'   \item{unemploy.lastyear}{PRE: SUMMARY: Unemployment better or worse in last year}
#'   \item{unemploy.nextyear}{PRE: More or less unemployment in next year}
#'   \item{unemploy.rate.now}{POST: What is the current unemployment rate}
#'   \item{univ.basic.income}{POST: SUMMARY: Favor/oppose federal program giving citizens $12K/year}
#'   \item{urban.unrest}{PRE: Best way to deal with urban unrest}
#'   \item{usa.better}{POST: SUMMARY: US better or wose than most other countries}
#'   \item{usa.on.track}{PRE: Are things in the country on right track}
#'   \item{usa.stay.home}{PRE: SUMMARY: Country would be better off if we just stayed home}
#'   \item{usa.stronger}{PRE: During last year, US position in world weaker or stronger}
#'   \item{vaccine.schools}{POST: SUMMARY: Favor/oppose requiring vaccines in schools}
#'   \item{vaccines.autism}{POST: Does most scientific evidence show vaccines cause autism or not}
#'   \item{vaccines.risk}{POST: SUMMARY: Health benefits of vaccinations outweigh risks}
#'   \item{violence.justified}{PRE: Justified to use violence}
#'   \item{vote.pres.str}{POST: Preference strong for Presidential candidate for whom R vote}
#'   \item{vote.when.decide}{POST: How long before election R made decision Presidential vote [coded]}
#'   \item{votes.accurate}{PRE: Votes counted accurately}
#'   \item{votes.faircount}{POST: How often are votes counted fairly}
#'   \item{voting.duty.choice}{PRE: SUMMARY: Voting as duty or choice}
#'   \item{voting.felons}{PRE: SUMMARY: Favor/oppose allowing felons to vote}
#'   \item{voting.id}{PRE: SUMMARY: Favor/oppose requiring ID when voting}
#'   \item{voting.mail}{PRE: SUMMARY: Favor/oppose vote by mail}
#'   \item{voting.rts.denied}{PRE: How often people denied right to vote}
#'   \item{waitvote2020}{POST: How long was wait time at polling place}
#'   \item{whenvote2020}{POST: When R voted in 2020 election}
#'   \item{whites.revdiscrim}{POST: How likely whites unable to find jobs because employers hiring minorities}
#'   \item{wom.complain.prob}{POST: Do women complaining about discrimination cause more problems}
#'   \item{wom.control.men}{PRE: Women seek to gain power by getting control over men}
#'   \item{wom.equal.spfav}{POST: Do women demanding equality seek special favors}
#'   \item{wom.interp.sexist}{PRE: Women interpret innocent remarks as sexist}
#'   \item{women.stay.honme}{POST: SUMMARY: Better/worse if man works and woman takes care of home}
#'   \item{work.employer}{PRE: Describe R's employment}
#'   \item{work.hoursweek}{PRE: How many hours R worked per week}
#'   \item{work.lastweek}{PRE: R worked for pay last week}
#'   \item{work.mom.bond}{POST: SUMMARY: Easier/harder for working mother to bond with child}
#'   \item{work.status}{PRE: SUMMARY: R occupation status 1 digit}
#'   \item{world.like.usa}{POST: Better if rest of world more like America}
#'   \item{world.temp.rising}{POST: Have world temperatuers have risen on average or last 100 years or not}
#'   \item{wt}{Full sample pre-election weight}
#'   \item{wt.post}{Full sample post-election weight} 
#' }
#' @source 2020 American National Election Survey.  See Appendix of printed textbook for further information
"nes"


#' Debate Experiment dataset for R Companion to Political Analysis, Third Edition
#'
#' A dataset with variables about students who participated in an experiment. This dataset is used to demonstrate application of R to political analysis. See book Appendix for variable names and descriptions.
#'
#' @format A data frame with 171 rows and 14 variables.
#' \describe{
#'   \item{obs}{Unique identification number for each subject}
#'   \item{assignment}{Name of condition subject was assigned to}
#'   \item{tv}{Did subject watch debate on TV? 1 = yes, 0 = no}
#'   \item{debinfo}{Number correct answers on five question quiz about the debate.}
#'   \item{catholic}{Is subject Catholic? 1 = yes, 0 = no}
#'   \item{issues}{Which candidate do you agree with on policy issues?}
#'   \item{integrity}{Which candidate has more integrity?}
#'   \item{leadership}{Which candidate is more effective leader?}
#'   \item{empathy}{Which candidate has more empathy?}
#'   \item{sophdum}{Is respondent a sophomore? 1 = yes, 0 = no.}
#'   \item{won}{Which candidate won the debate? 1 = Kennedy ... 4 = Tie ... 7 = Nixon}
#'   \item{pid}{Self-reported partisan identification on standard 1-7 scale}
#'   \item{ideology}{Self-reported political ideology on standard 1-7 scale}
#'   \item{gender}{Subject's gender, 0 = male, 1 = female.}
#' }
#' @source Jamie Druckman.  See Appendix of printed textbook for further information.
"debate"


#' Dataset for Chapter 6 Exercises, An R Companion to Political Analysis, Third Edition
#'
#' A hypothetical dataset of human subject research
#'
#' @format A data frame with 100 rows and 7 variables.
#' \describe{
#'   \item{id}{Unique identification number for each subject}
#'   \item{subject_name}{Last name of subject}
#'   \item{age}{Age of subject in years}
#'   \item{partyid}{Self-identified party identification from pretest}
#'   \item{prior_classes}{Number of political science classes subject has completed}
#'   \item{site_viewed}{Did subject view the candidate's old or new site in the experiment?}
#'   \item{site_rating}{Subject's rating of candidate on feeling thermometer after viewing web site}
#' }
#' @source Barry C. Edwards and Philip H. Pollock, III
#' @keywords internal
"ch6ex"
