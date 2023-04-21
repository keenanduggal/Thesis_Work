%% Simple SIR Model: 

% Susceptibles = D(1); Infected = D(2); Recovered = D(3)
% beta = transmission rate; gamma = recovery rate; 
    % force of infection = beta * I / N --> Assumes frequency dependent transmission 


% Parameters 
tspan = [0, 1];
ic = [87, 3, 10];
beta = 65; 
gamma = 10;
recruit_rate = 33;
mortality_rate = 0.15;
disease_mortality_rate = 0.15;


dPdt =  @(t,D)[
recruit_rate - beta * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - mortality_rate * D(1); % dSdt
beta * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate+gamma + disease_mortality_rate)*D(2); % dIdt
gamma * D(2) - mortality_rate * D(3) % dRdt
];
[t,plants] = ode15s(dPdt, tspan, ic);

plot(t,plants(:,1), 'b', 'LineWidth',2, 'DisplayName', 'Susceptible'), hold on
plot(t,plants(:,2), 'k', 'LineWidth',2,'DisplayName', 'Infected')
plot(t,plants(:,3), 'r', 'LineWidth',2,'DisplayName', 'Recovered'), grid on
legend('FontSize',14);
xlabel('Progression through Single Season', 'FontSize',14)
ylabel('# Plants', 'FontSize',14)
ylim([0,100])
title('Simple SIR Model', 'FontSize',24);


%% Deterministic SIR w/ multiple years + climate change 

% Notes: 
    % - Currently assumes 5/8 of recovered plants become susceptible again after a winter
    % - Force of infection = beta * I / N --> Assumes frequency dependent transmission 
    % Isolated (no new disease colonization, + Infection rachets (doesn't
    % decrease over winter) 
    % Assumes fixed recruitment rate 

% Starting Population Parameters 
susceptible = 87;
infected = 3; 
recovered = 10; 
tspan = [0, 1]; % Integrates over one season
beta = 65; 
gamma = 10;
recruit_rate = 33;
mortality_rate = 0.15;
disease_mortality = 0.15; 
plant_survival_over_winter = 0.85;
carrying_capacity = 300; 

% Storage Vectors: 
susceptible_stored = []; 
infected_stored = []; 
recovered_stored = []; 

% Iterating through x years: 
x = 100; 
for i = 1:x 
ic = [susceptible, infected, recovered]; % Reset Starting conditions 

%%% System of Differential Equations:

% Normal Baseline: No climate effects 
dPdt =  @(t,D)[
recruit_rate - beta * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - mortality_rate * D(1); % dS/dt
beta * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate+disease_mortality+gamma)*D(2); % dI/dt
gamma * D(2) - mortality_rate * D(3)]; % dR/dt

% Pessimistic Scenario: Increased Temperatures: increase plant mortality,
% increase pathogen infectivity, decreased recovery, lower fecundity  
dP1dt =  @(t,D)[
(recruit_rate - 0.1*i) - (beta + i) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + (i ./ (i + 2 * x)))  * D(1); % dS/dt
(beta + i ) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + disease_mortality + (i ./ (i + 2 * x))+(gamma - (i ./ (i + 2 * x))))*D(2); % dI/dt
(gamma - (i ./ (i + 2 * x))) * D(2) - (mortality_rate + (i ./ (i + 2 * x))) * D(3)]; % dR/dt

% Optimistic Scenario: Increased Temperatures: decreased plant mortality,
% decreased pathogen infectivity, increased recovery, higher fecundity  
dP2dt =  @(t,D)[
(recruit_rate + 0.1*i) - (beta - i) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate - (i ./ (i + 2 * x)))  * D(1); % dS/dt
(beta - i ) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + disease_mortality - (i ./ (i + 2 * x))+(gamma + (i ./ (i + 2 * x))))*D(2); % dI/dt
(gamma + (i ./ (i + 2 * x))) * D(2) - (mortality_rate - (i ./ (i + 2 * x))) * D(3)]; % dR/dt

% Realistic Scenario: Increased Temperatures: slightly higher plant
% mortality, increased infectivity (up to a point, then decreased), lower
% fecundity, decreased recovery 
if i >=  25 % Climate effect on transmission 
    beta_adjust =  -0.25 * (i-25); 
else 
    beta_adjust =  0.25 * i;
end 

dP3dt =  @(t,D)[
(recruit_rate - 0.025*i) - (beta + beta_adjust) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + (i ./ (i + 2 * x))) * D(1); % dS/dt
(beta + beta_adjust) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - ((mortality_rate +disease_mortality + (i ./ (i + 2 * x)))+(gamma - (i ./ (i + 2 * x))))*D(2); % dI/dt
(gamma - (i ./ (i + 2 * x))) * D(2) - (mortality_rate + (i ./ (i + 2 * x))) * D(3)]; % dR/dt


% Select which set of differential equations to use 
[t,plants] = ode15s(dP3dt, tspan, ic);

% Adds growing season data + fall, winter, spring linear declines into storage arrays: 
length_fws = size(plants(:, 1), 1) * 3; 
susceptible_over_winter = transpose(linspace(plants(end, 1),plants(end, 1) * plant_survival_over_winter, length_fws));
infected_over_winter = transpose(linspace(plants(end, 2),plants(end, 2) * plant_survival_over_winter, length_fws));
recovered_over_winter = transpose(linspace(plants(end, 3),plants(end, 3) * plant_survival_over_winter, length_fws));

% Aggregates annual data vector to previous years: 
susceptible_stored = cat(1, susceptible_stored, plants(:,1), susceptible_over_winter); 
infected_stored = cat(1, infected_stored, plants(:,2), infected_over_winter); 
recovered_stored = cat(1, recovered_stored, plants(:,3), recovered_over_winter); 

% Update Population Numbers for Start of next season 
susceptible = plant_survival_over_winter * (plants(end,1) + (5/8) * plants(end,3));
infected = plant_survival_over_winter * plants(end, 2);
recovered = plant_survival_over_winter * ((3/8) * plants(end,3));

end

y = infected_stored(:,1) + susceptible_stored(:,1) + recovered_stored(:,1); 
idx = y>carrying_capacity;
y(idx) = 300;

% Visualizing Results: 
f2 = figure;
figure(f2);
plot(transpose(1:size(infected_stored, 1)) / (size(infected_stored, 1) / x), y, 'k', 'LineWidth',3, 'DisplayName', 'All Plants')
xlabel('Time (years)', 'FontSize',14)
ylabel('# Plants', 'FontSize',14)
yl = ylim; 
ylim([0, yl(2)]);
title('Total # Plants', 'FontSize',24);


% plot(transpose(1:size(infected_stored, 1)) / (size(infected_stored, 1) / x),infected_stored(:,1))

%{
f1 = figure;
figure(f1);
plot(transpose(1:size(susceptible_stored, 1)) / (size(susceptible_stored, 1) / x), susceptible_stored(:,1), 'b', 'DisplayName', 'Susceptible', 'LineWidth',1), hold on
plot(transpose(1:size(infected_stored, 1)) / (size(infected_stored, 1) / x), infected_stored(:,1), 'k', 'DisplayName', 'Infected', 'LineWidth',1)
plot(transpose(1:size(recovered_stored, 1)) / (size(recovered_stored, 1) / x), recovered_stored(:,1), 'r', 'DisplayName', 'Recovered', 'LineWidth',1), grid on
legend('FontSize',14);
xlabel('Time (years)', 'FontSize',14)
ylabel('# Plants', 'FontSize',14)
ylim([0,100])
title('Tracking SIR Dynamics', 'FontSize',24);
%}
%% Running Multiple Simulations of Stochastic Model: 

% New Notes: 
    % - Percentage of infection (not infected plants!!) that 
    % survives through winter is random (between 0 - 100%), no longer fixed at 100%
    % - Accounts for random new disease colonization events
    %   (currently simplifying assumption is that this only takes place
    %   at the beginning of a season)
    % Stochasticity in each parameter = introduced 


   
f = figure; % Creates final plotting figure 

%Storing sampled points from stochastic simulations to later average: 
suc = []; 
inf = [];
rec = [];

n = 10; % Number of simulations
for k = 1:n 

% Fixed Starting Population Parameters 
susceptible = 87;
infected = 3; 
recovered = 10; 
tspan = [0, 1]; % Integrates over one season

% Stable
beta = 65; 
gamma = 10;
recruit_rate = 33;
mortality_rate = 0.15;
disease_mortality = 0.15; 
plant_survival_over_winter = 0.85;


% Storage Vectors for Each Simulation: 
susceptible_stored = []; 
infected_stored = []; 
recovered_stored = []; 

% Iterating through x years: 
x = 100; 
for i = 1:x 
ic = [susceptible, infected, recovered]; % Reset Starting conditions 

rndm = rand(8,1); % Uniform random distribution 
if rndm(1) >= 0.5 % Birth Rate Stochasticity 
    effect = 5; 
else 
    effect = -5; 
end 
if rndm(3) >= 0.5 % Transmission Rate Stochasticity 
    effect2 = 3.25; 
else 
    effect2 = -3.25; 
end 
if rndm(5) >= 0.5 % Mortality Rate Stochasticity 
    effect3 = 0.01; 
else 
    effect3 = -0.01; 
end 
if rndm(7) >= 0.5 % Recovery Rate Stochasticity 
    effect4 = 0.5; 
else 
    effect4 = -0.5; 
end 

if i >=  25 % Climate effect on transmission 
    beta_adjust =  -0.1 * (i-25); 
else 
    beta_adjust =  0.1 * i;
end 

% No climate effects
dPdt_noclimate =  @(t,D)[
(recruit_rate + effect*rndm(2)) - (beta + effect2*rndm(4)) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + effect3*rndm(6)) * D(1); % dS/dt
(effect2*rndm(4) + beta) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - ((mortality_rate + effect3*rndm(6) + disease_mortality)+(gamma + effect4*rndm(8)))*D(2); % dI/dt
(gamma + effect4*rndm(8)) * D(2) - (mortality_rate + effect3*rndm(6)) * D(3)]; % dR/dt

% Predicted climate scenario: slightly higher plant
% mortality, increased infectivity (up to a point, then decreased), lower
% fecundity, decreased recovery, + Stochasticity (encompasses environment + natural parameter): 
dPdt_climate =  @(t,D)[
(recruit_rate + effect*rndm(2) - (0.01 * i)) - (beta + beta_adjust + effect2*rndm(4)) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + effect3*rndm(6) + 0.001*i) * D(1); % dS/dt
(effect2*rndm(4) + beta + beta_adjust) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - ((mortality_rate + effect3*rndm(6) + disease_mortality + 0.001*i)+(gamma + effect4*rndm(8) - 0.01 * i))*D(2); % dI/dt
(gamma + effect4*rndm(8) - 0.01 * i) * D(2) - (mortality_rate + effect3*rndm(6) + 0.001*i) * D(3)]; % dR/dt

% Pessimistic Scenario: Increased Temperatures: increase plant mortality,
% increase pathogen infectivity, decreased recovery, lower fecundity  
dPdt_pessimistic =  @(t,D)[
(recruit_rate + effect*rndm(2) - 0.1*i) - (beta + i + effect2*rndm(4)) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + effect3*rndm(6) + (i ./ (i + 2 * x)))  * D(1); % dS/dt
(beta + i + effect2*rndm(4)) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + effect3*rndm(6) + disease_mortality + (i ./ (i + 2 * x))+(gamma + effect4*rndm(8) - (i ./ (i + 2 * x))))*D(2); % dI/dt
(gamma + effect4*rndm(8) - (i ./ (i + 2 * x))) * D(2) - (mortality_rate + effect3*rndm(6) + (i ./ (i + 2 * x))) * D(3)]; % dR/dt

% Optimistic Scenario: Increased Temperatures: decreased plant mortality,
% decreased pathogen infectivity, increased recovery, higher fecundity  
dPdt_optimistic =  @(t,D)[
(recruit_rate + effect*rndm(2) + 0.1*i) - (beta + effect2*rndm(4) - i) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + effect3*rndm(6) - (i ./ (i + 2 * x)))  * D(1); % dS/dt
(beta + effect2*rndm(4) - i ) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + effect3*rndm(6) + disease_mortality - (i ./ (i + 2 * x))+(gamma + effect4*rndm(8) + (i ./ (i + 2 * x))))*D(2); % dI/dt
(gamma + effect4*rndm(8) + (i ./ (i + 2 * x))) * D(2) - (mortality_rate + effect3*rndm(6) - (i ./ (i + 2 * x))) * D(3)]; % dR/dt

% Select which scenario of differential equations to use 
[t,plants] = ode15s(dPdt_climate, tspan, ic);

% Adds growing season data + fall, winter, spring linear declines into storage arrays: 
length_fws = size(plants(:, 1), 1) * 3; 
susceptible_over_winter = transpose(linspace(plants(end, 1),plants(end, 1) * plant_survival_over_winter, length_fws));
infected_over_winter = transpose(linspace(plants(end, 2),plants(end, 2) * plant_survival_over_winter, length_fws));
recovered_over_winter = transpose(linspace(plants(end, 3),plants(end, 3) * plant_survival_over_winter, length_fws));

% Aggregates annual data vector to previous years: 
susceptible_stored = cat(1, susceptible_stored, plants(:,1), susceptible_over_winter); 
infected_stored = cat(1, infected_stored, plants(:,2), infected_over_winter); 
recovered_stored = cat(1, recovered_stored, plants(:,3), recovered_over_winter); 

% Accounting for new disease colonization: 
col_chance = rand; 
if col_chance < 0.5
    colonization = 0; 
elseif col_chance < (2/3)
    colonization = 1; 
elseif col_chance < (5/6)
    colonization = 3; 
else 
    colonization = 5; 
end

% Update Population Numbers for Start of next season 
inf_surv = rand/1.11112 + 0.1; 
susceptible = plant_survival_over_winter * (plants(end,1) + (5/8) * plants(end,3) + plants(end,2) * (1-inf_surv)) - colonization;
infected = plant_survival_over_winter * (plants(end, 2) * inf_surv) + colonization;
recovered = plant_survival_over_winter * ((3/8) * plants(end,3));

end

% Updates master storage vectors in order to average stochastic simulations
    % ODE solver is variable length which --> different size vectors for each
    % simulation. Workaround = percentiles of data: 
    data_percentiles = transpose(0.01:0.01:1); 
    sample_points = cat(1, 1, round(data_percentiles * size(susceptible_stored(:,1), 1)));
    susceptible_full = susceptible_stored(:,1); 
        susceptible_sampled = susceptible_full(sample_points);
        suc = cat(2, suc, susceptible_sampled);
    infected_full = infected_stored(:,1); 
        infected_sampled = infected_full(sample_points);
        inf = cat(2, inf, infected_sampled);
    recovered_full = recovered_stored(:,1); 
        recovered_sampled = recovered_full(sample_points);
        rec = cat(2, rec, recovered_sampled);

figure(f); % Plot each simulation: 
plot(transpose(1:size(infected_stored, 1)) / (size(infected_stored, 1) / x), infected_stored(:,1) + susceptible_stored(:,1) + recovered_stored(:,1), 'LineWidth',1, 'DisplayName', 'All Plants'), hold on 
end

% Finish plotting each simulation: 
grid on
xlabel('Time (years)', 'FontSize',14)
ylabel('Total # Plants', 'FontSize',14)
yl = ylim; 
ylim([0, yl(2)]);
title('Stochastic Simulations', 'FontSize',24);

% Plot Average of each Simulation: 
f2 = figure;
figure(f2); 
average_data = mean(suc,2) + mean(inf, 2) + mean(rec, 2); 
data_sterrorofmean  = std(suc, 0, 2)/sqrt(n) + std(inf, 0, 2)/sqrt(n) + std(rec, 0, 2)/sqrt(n);
lngth = size(average_data, 1);
x_values = transpose((1:lngth) / (lngth / x));
plot(x_values, average_data, 'b', 'LineWidth',3, 'DisplayName', 'Average of Simulations'), hold on 
    % Plot Standard Error of Mean As Shaded Area: 
    curve1 = average_data + data_sterrorofmean;
    curve2 = average_data - data_sterrorofmean; 
    plot(x_values, curve1, 'r', x_values, curve2, 'r'), grid on

xlabel('Time (years)', 'FontSize',14)
ylabel('Total # Plants', 'FontSize',14)
yl = ylim; 
ylim([0, yl(2)]);
title('Average of Simulations', 'FontSize',24);


%% Changing Parameter Space:  

% To use: change one paramater_adjust (e.g., mortality_adjust) at a time 
    % make sure to change within ode function to call mortality_adjust(r)
    % make sure when graphing to change code accordingly 


pop_change_storage = [];
average_data_storage = [];
infected_data_storage = [];

for r = 1:10

%Storing sampled points from stochastic simulations to later average: 
suc = []; 
inf = [];
rec = [];

n = 25; % Number of simulations
for k = 1:n 

% Fixed Starting Population Parameters 

Population_size_multiplier = transpose(0.3:0.18:2);
susceptible = 87 * Population_size_multiplier(r);
infected = 3 * Population_size_multiplier(r); 
recovered = 10 * Population_size_multiplier(r); 
tspan = [0, 1]; % Integrates over one season

% Stable
beta = 65; % * Population_size_multiplier(r); 
gamma = 10; % * Population_size_multiplier(r);
recruit_rate = 33; % * Population_size_multiplier(r);
mortality_rate = 0.15;
disease_mortality = 0.15; 
plant_survival_over_winter = 0.85;


% Storage Vectors for Each Simulation: 
susceptible_stored = []; 
infected_stored = []; 
recovered_stored = []; 

% Iterating through x years: 
x = 100; 
for i = 1:x 
ic = [susceptible, infected, recovered]; % Reset Starting conditions 

rndm = rand(8,1); % Uniform random distribution 
if rndm(1) >= 0.5 % Birth Rate Stochasticity 
    effect = 5; 
else 
    effect = -5; 
end 
if rndm(3) >= 0.5 % Transmission Rate Stochasticity 
    effect2 = 3.25; 
else 
    effect2 = -3.25; 
end 
if rndm(5) >= 0.5 % Mortality Rate Stochasticity 
    effect3 = 0.01; 
else 
    effect3 = -0.01; 
end 
if rndm(7) >= 0.5 % Recovery Rate Stochasticity 
    effect4 = 0.5; 
else 
    effect4 = -0.5; 
end 


% No climate effects
dPdt_noclimate =  @(t,D)[
(recruit_rate + effect*rndm(2)) - (beta + effect2*rndm(4)) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + effect3*rndm(6)) * D(1); % dS/dt
(effect2*rndm(4) + beta) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - ((mortality_rate + effect3*rndm(6) + disease_mortality)+(gamma + effect4*rndm(8)))*D(2); % dI/dt
(gamma + effect4*rndm(8)) * D(2) - (mortality_rate + effect3*rndm(6)) * D(3)]; % dR/dt

% Predicted climate scenario: slightly higher plant
% mortality, increased infectivity (up to a point, then decreased), lower
% fecundity, decreased recovery, + Stochasticity (encompasses environment + natural parameter): 

% Parameters to test: 

% recruit_adjust = transpose(0.002:0.02:0.2); % Use when testing 
recruit_adjust = 0.01;


% When testing, swap 0.1 with transpose(0.0086:.086:0.86)
if i >=  25 % Climate effect on transmission 
    beta_adjust =  0.1 * (i-25); 
else 
    beta_adjust = 0.1 * i;
end
beta_adjust_raw = transpose(0.0086:.086:0.86); % For graphing purposes


% mortality_adjust = transpose(0.0005:0.0005:0.005); % Use when testing 
mortality_adjust = 0.001;

% gamma_adjust = transpose(0.0075:0.0075:0.075);
gamma_adjust = 0.01;


% For greater effects of climate change on diseased plants, multiply
% mortality adjust in Infected equation by number 
dPdt_climate =  @(t,D)[
(recruit_rate + effect*rndm(2) - recruit_adjust * i) - (beta + beta_adjust + effect2*rndm(4)) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + effect3*rndm(6) + mortality_adjust * i) * D(1); % dS/dt
(effect2*rndm(4) + beta + beta_adjust) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - ((mortality_rate + effect3*rndm(6) + disease_mortality + mortality_adjust * 1 *i)+(gamma + effect4*rndm(8) - gamma_adjust * i))*D(2); % dI/dt
(gamma + effect4*rndm(8) - gamma_adjust * i) * D(2) - (mortality_rate + effect3*rndm(6) + mortality_adjust * i) * D(3)]; % dR/dt


% Select which scenario of differential equations to use 
[t,plants] = ode15s(dPdt_climate, tspan, ic);

% Adds growing season data + fall, winter, spring linear declines into storage arrays: 
length_fws = size(plants(:, 1), 1) * 3; 
susceptible_over_winter = transpose(linspace(plants(end, 1),plants(end, 1) * plant_survival_over_winter, length_fws));
infected_over_winter = transpose(linspace(plants(end, 2),plants(end, 2) * plant_survival_over_winter, length_fws));
recovered_over_winter = transpose(linspace(plants(end, 3),plants(end, 3) * plant_survival_over_winter, length_fws));

% Aggregates annual data vector to previous years: 
susceptible_stored = cat(1, susceptible_stored, plants(:,1), susceptible_over_winter); 
infected_stored = cat(1, infected_stored, plants(:,2), infected_over_winter); 
recovered_stored = cat(1, recovered_stored, plants(:,3), recovered_over_winter); 

% Accounting for new disease colonization: 

col_chance = rand; 
if col_chance < 0.5
    colonization = 0; 
elseif col_chance < (2/3)
    colonization = 1; 
elseif col_chance < (5/6)
    colonization = 3; 
else 
    colonization = 5; 
end

%colonization = 1.5:1.5:15; 


% Update Population Numbers for Start of next season 
inf_surv = rand/1.11112 + 0.1; 
susceptible = plant_survival_over_winter * (plants(end,1) + (5/8) * plants(end,3) + plants(end,2) * (1-inf_surv)) - colonization;
infected = plant_survival_over_winter * (plants(end, 2) * inf_surv) + colonization;
recovered = plant_survival_over_winter * ((3/8) * plants(end,3));

end

% Updates master storage vectors in order to average stochastic simulations
    % ODE solver is variable length which --> different size vectors for each
    % simulation. Workaround = percentiles of data: 
    data_percentiles = transpose(0.01:0.01:1); 
    sample_points = cat(1, 1, round(data_percentiles * size(susceptible_stored(:,1), 1)));
    susceptible_full = susceptible_stored(:,1); 
        susceptible_sampled = susceptible_full(sample_points);
        suc = cat(2, suc, susceptible_sampled);
    infected_full = infected_stored(:,1); 
        infected_sampled = infected_full(sample_points);
        inf = cat(2, inf, infected_sampled);
    recovered_full = recovered_stored(:,1); 
        recovered_sampled = recovered_full(sample_points);
        rec = cat(2, rec, recovered_sampled);

end


average_data = mean(suc,2) + mean(inf, 2) + mean(rec, 2); 
infected_data = mean(inf, 2);
net_pop_change = average_data(end) - average_data(1);
pop_change_storage = cat(1, pop_change_storage, net_pop_change); 
average_data_storage = cat(2, average_data_storage, average_data);
infected_data_storage = cat(2, infected_data_storage, infected_data);
end


cMap = interp1([0;1],[0 1 0; 1 0 0],linspace(0,1,10));
labels = string(Population_size_multiplier); % Change this to reflect parameter being tested
figure 
hold on
for z = 1:r
plot(1:length(average_data_storage(:,z)),average_data_storage(:,z), 'Color', cMap(z,:), 'LineWidth',2, 'DisplayName', labels(z))
end 
grid on
xlabel('Time', 'FontSize', 18)
ylabel('Population Size', 'FontSize', 18)
ax=gca;
ax.FontSize = 20;
title("Proportional Populations", 'FontSize',24);
subtitle("With Predicted Climate Effects", 'FontSize',16)
legend(labels);
 

figure
hold on
for z = 1:r
plot(1:length(infected_data_storage(:,z)),infected_data_storage(:,z), 'Color', cMap(z,:), 'LineWidth',2, 'DisplayName', labels(z))
end 
grid on
xlabel('Time', 'FontSize', 18)
ylabel('Diseased Population Size', 'FontSize', 18);
ax=gca;
ax.FontSize = 20;
title("Varying Colonization", 'FontSize',24);
% subtitle("With Predicted Climate Effects", 'FontSize',16)
legend(labels);


%% Temperature / Climate over Time: 

x = transpose(0:1:100);
y = [transpose(0:1:100), transpose(0:1.1:110), transpose(0:1.2:120), transpose(0:1.3:130), transpose(0:1.4:140), transpose(0:1.5:150), transpose(0:1.6:160), transpose(0:1.75:175), transpose(0:1.85:185), transpose(0:2:200)];
cMap = interp1([0;1],[0 1 0; 1 0 0],linspace(0,1,10));
labels = string(1:10);

figure
hold on
for z = 1:10
plot(x,y(:,z), 'Color', cMap(z,:), 'LineWidth',2, 'DisplayName', labels(z))
end 
grid on
xlabel('Time (Years)', 'FontSize', 18)
ylabel('"Climate"', 'FontSize', 18);
ax=gca;
ax.FontSize = 20;
set(gca,'ytick',[])
title("Climate Change Over Time", 'FontSize',24);
legend(labels);

