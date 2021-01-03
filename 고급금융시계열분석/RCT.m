clc
clear all
close all

%% data loading

fakeData = readtable('RCT_fakeData.xlsx'); 
% subject number, vaccine treatment dummy, corona positive dummy

%% test

y = fakeData.CoronaPositive;
X = [ones(size(y)) fakeData.vaccineTreatment];
coeff = (X'*X) \ (X'*y);
residual = y - X*coeff;

% comparison of results
coeff(2)
mean(fakeData.CoronaPositive ( fakeData.vaccineTreatment == 1 )) - ...
	mean(fakeData.CoronaPositive ( fakeData.vaccineTreatment == 0 ))  %#ok<NOPTS>
    
N_treated = sum( fakeData.vaccineTreatment == 1 );
N = length(fakeData.vaccineTreatment);
N_controlled = N - N_treated;

tStat = sqrt(N)*coeff(2) / sqrt( (N/N_treated + N/N_controlled)*var(residual) );
