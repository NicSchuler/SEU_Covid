---
title: "Savage Expected Utility"
author: "J.Kämpfer, R.Scherrer & N.Schuler"
date: "04 12 2020"
output: html_document
---


## Utility Function

It is assumed, that the swiss federal council has a utility function, which decreases in economic costs and in so-called health costs, which assign a monetary value to the lost years of lifetime through a Covid-19 death. It is further assumed, that the economic and the health costs are weighted equally and that the federal council is risk neutral.

Following utility function will therefore be used for further evaluation:

$$u(EconomicCost, HealthCost) = -(EconomicCost+HealthCost)$$
The result can be interpreted as the expected monetary loss equivalent of economic cost and lost years of lifetime.

For comparison, a second, very similar but risk averse utility funcion can be evaluated:
$$u(EconomicCost, HealthCost) = -(EconomicCost+HealthCost)^2$$
