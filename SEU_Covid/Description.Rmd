---
title: "Savage Expected Utility"
author: "J.Kämpfer, R.Scherrer & N.Schuler"
date: "04 12 2020"
output: html_document
---

## Savage expected utility

Description of SEU tbd.

## General modelling set up

This application shall assess the swiss federal council's decision making in terms of measures against the Covid-19 pandemic in March 2020.

In order to assess the impact of the Covid-19 pandemic, we will model the economic and heath aspects separately and bring them together afterwards.

### Modelling the Economic Impact

The economic impact shall be evaluated based on the costs, which occur through the different states. It is assumed, that the costs of the pandemic, which are taken into account by the swiss federal council, only consist of the additional costs (compared to a world without Covid-19), that are payed by the swiss government (e.g. difference in governmental spending due to Covid-19). The costs are assumed to be:

$$Cost = 0.8*(u^*-u^e)*wage*duration + subventions + d*CreditVolume$$
with:
* $u^*$ being the amount of unemployed people (incl. short-time work ("Kurzarbeit"));
* $u^e$ being the amount of unemployed people expected, if no pandempic would happen (assumed to remain constant compared to 2019, 2.3% according to; [seco](https://www.seco.admin.ch/seco/de/home/seco/nsb-news/medienmitteilungen-2020.msg-id-77726.html#:~:text=Bern%2C%2010.01.2020%20%2D%20Das,Arbeitslosenquote%20von%202%2C3%25.));
* $wage$ is the average monthly wage in Switzerland (6656 CHF, nominally adjusted to 2018 levels, according to [bfs](https://www.bfs.admin.ch/bfs/de/home/statistiken/arbeit-erwerb/loehne-erwerbseinkommen-arbeitskosten.html));
* $duration$ is the average length of additional unemployment in months;
* $subventions$ are directly payed, non refundable payments to aid firms, institutions, clubs etc;
* $d$ is the credit default rate on governmentally backed credits;
* $CreditVolume$ is the volume of credits backed by the government.

Modelling tbd.

### Modelling the Health Impact

tbd

## Bringing all together

It is assumed, that the swiss federal council has a utility function, which decreases in costs and Covid-19 deaths. Further it is assumed, that this relation is linear, which also implies a fixed trade-off between deaths and cases, which is denoted $v$.

Following utility function will therefore be used for further evaluation:

$$u(cost, deaths) = -(\frac{1}{v}*cost+deaths)$$

Whereas $v$ is the *value of statistical life (VOSL)*, which was calculated based on the swiss mortality table of 2005, the value of a lost year of lifetime and the distribution of Covid-19 deaths per age group in China.

$v$ can also be expressed by $AvgLostYears * VOSLperYear$, which means the average of lost years of lifetime for Covid-19 deaths multiplied with the value of a lost year of lifetime.

Under the assumtion, that the Covid-19 deaths per age group in Switzerland are similar to the distribution measured in China (China CDC Weekly, 02/2020), 16.89 years of lifetime are lost on average. With a value of 100'000 CHF/Year as supposed by [Quelle tbd], $v$ should be assumed to be equal to $1'689'656$ CHF.




