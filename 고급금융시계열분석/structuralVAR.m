clc
clear all
close all

%% MSFT data loading

microsoftData = readtable('msftData.xlsx');

returnsVolume = [(100*microsoftData.Returns) (microsoftData.Volume/10^6)];

y = returnsVolume(2:end,:);
X = [ones(length(y),1) returnsVolume(1:end-1,:)];

%% reduced-form VAR

coeff = (X'*X) \ (X'*y);
residual = y - X*coeff;
covarianceMatrix = cov(residual);

constant = coeff(1,:)';
AR1_matrix = coeff(2:end,:)';


%% structural VAR (a very very simple case)

L = chol( covarianceMatrix )';

%% IRF: impulse (1SD trading volume) response (of future trading volume)  
impulse = [0 1]'; % 1SD of trading volume

IRF_horizon = 10;
response = nan(IRF_horizon,2) ;

% initialize impulse
currentImpulse = L*impulse;
currentVariance = zeros(2,2);

response(1,1) = currentImpulse(2);
response(1,2) = currentVariance(2,2);

previousImpulse = currentImpulse;
previousVariance = currentVariance;

for t = 2:IRF_horizon
   
    currentImpulse = AR1_matrix*previousImpulse;
    currentVariance = AR1_matrix*previousVariance*AR1_matrix' + covarianceMatrix;
    
    response(t,1) = currentImpulse(2);
    response(t,2) = currentVariance(2,2);

    previousImpulse = currentImpulse;
    previousVariance = currentVariance;
    
end

figure(1)

plot((1:IRF_horizon),response(:,1)', 'k', 'LineWidth',3 )
hold on
plot((1:IRF_horizon),response(:,1)' + 1.96*sqrt(response(:,2))' , 'r', 'LineWidth',1)
hold on
plot((1:IRF_horizon),response(:,1)' - 1.96*sqrt(response(:,2))' , 'r', 'LineWidth',1)

xlabel('time')
ylabel('response of trading volume')

title('IRF')


