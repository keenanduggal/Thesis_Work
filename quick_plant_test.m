%% Simple SIR Model: 

% Susceptibles = D(1); Infected = D(2); Recovered = D(3)
% N = total population size
% beta = transmission rate; gamma = recovery rate; 
    % force of infection = beta * I / N --> Assumes frequency dependent transmission 


% Parameters 
tspan = [0, 1];
ic = [100, 10, 0];
beta = 25; 
gamma = 1;
birth_rate = 7.5;
mortality_rate = 0.4;


dPdt =  @(t,D)[
birth_rate - beta * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - mortality_rate * D(1); % dSdt
beta * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate+gamma)*D(2); % dIdt
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

%% SIR Simulation Over Multiple Seasons: 

% Notes: 
    % Currently assumes 5/8 of recovered plants become susceptible again after a winter
    % Also assumes stasis between seasons 


% Starting Population Parameters 
susceptible = 100;
infected = 1; 
recovered = 0; 
tspan = [0, 1];
beta = 70; 
gamma = 2;
birth_rate = 7.5;
mortality_rate = 0.1;
disease_mortality = 0.3; 

% Storage Vectors: 
susceptible_stored = []; 
infected_stored = []; 
recovered_stored = []; 

% Iterating through x years: 
x = 10; 
for i = 1:x 
ic = [susceptible, infected, recovered]; % Reset Starting conditions 

% System of Differential Equations
dPdt =  @(t,D)[
birth_rate - beta * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - mortality_rate * D(1); % dS/dt
beta * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + disease_mortality +gamma)*D(2); % dI/dt
gamma * D(2) - mortality_rate * D(3)]; % dR/dt

[t,plants] = ode15s(dPdt, tspan, ic);

% Adds annual data + fall, winter, spring stasis into storage arrays: 
susceptible_stored = cat(1, susceptible_stored, plants(:,1), transpose(repelem(plants(end, 1), size(plants(:, 1), 1) * 3))); 
infected_stored = cat(1, infected_stored, plants(:,2), transpose(repelem(plants(end, 2), size(plants(:, 2), 1) * 3))); 
recovered_stored = cat(1, recovered_stored, plants(:,3), transpose(repelem(plants(end, 3), size(plants(:, 3), 1) * 3))); 

% Update Population Numbers for Start of next season 
susceptible = plants(end,1) + (5/8) * plants(end,3);
infected = plants(end,2);
recovered = (3/8) * plants(end,3);

end

% Visualizing Results: 
f1 = figure;
f2 = figure;
figure(f1);
plot(transpose(1:size(susceptible_stored, 1)) / (size(susceptible_stored, 1) / x), susceptible_stored(:,1), 'b', 'DisplayName', 'Susceptible'), hold on
plot(transpose(1:size(infected_stored, 1)) / (size(infected_stored, 1) / x), infected_stored(:,1), 'k', 'DisplayName', 'Infected')
plot(transpose(1:size(recovered_stored, 1)) / (size(recovered_stored, 1) / x), recovered_stored(:,1), 'r', 'DisplayName', 'Recovered'), grid on
xlabel('Time (years)', 'FontSize',14)
ylabel('# Plants', 'FontSize',14)
ylim([0,100])
legend;
title('Tracking SIR Dynamics', 'FontSize',24);

figure(f2);
plot(transpose(1:size(infected_stored, 1)) / (size(infected_stored, 1) / x), infected_stored(:,1) + susceptible_stored(:,1) + recovered_stored(:,1), 'k', 'DisplayName', 'All Plants')
xlabel('Time (years)', 'FontSize',14)
ylabel('# Plants', 'FontSize',14)
ylim([0,100])
title('Total # Plants', 'FontSize',24);



%% SIR w/ multiple years + climate change 

% Starting Population Parameters 
susceptible = 100;
infected = 1; 
recovered = 0; 
tspan = [0, 1];
beta = 70; 
gamma = 2;
birth_rate = 7.5;
mortality_rate = 0.1;
disease_mortality = 0.3; 


% Storage Vectors: 
susceptible_stored = []; 
infected_stored = []; 
recovered_stored = []; 

% Iterating through x years: 
x = 50; 
for i = 1:x 
ic = [susceptible, infected, recovered]; % Reset Starting conditions 

%%% System of Differential Equations:

% Normal Baseline: No climate effects 
dPdt =  @(t,D)[
birth_rate - beta * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - mortality_rate * D(1); % dS/dt
beta * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate+disease_mortality+gamma)*D(2); % dI/dt
gamma * D(2) - mortality_rate * D(3)]; % dR/dt

% Pessimistic Scenario: Increased Temperatures: increase plant mortality,
% increase pathogen infectivity, decreased recovery, lower fecundity  
dP1dt =  @(t,D)[
(birth_rate - 0.1*i) - (beta + i ) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + (i ./ (i + 2 * x)))  * D(1); % dS/dt
(beta + i ) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + disease_mortality + (i ./ (i + 2 * x))+(gamma - (i ./ (i + 2 * x))))*D(2); % dI/dt
(gamma - (i ./ (i + 2 * x))) * D(2) - (mortality_rate + (i ./ (i + 2 * x))) * D(3)]; % dR/dt

% Optimistic Scenario: Increased Temperatures: decreased plant mortality,
% decreased pathogen infectivity, increased recovery, higher fecundity  
dP2dt =  @(t,D)[
(birth_rate + 0.1*i) - (beta - i) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate - (i ./ (i + 2 * x)))  * D(1); % dS/dt
(beta - i ) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + disease_mortality - (i ./ (i + 2 * x))+(gamma + (i ./ (i + 2 * x))))*D(2); % dI/dt
(gamma + (i ./ (i + 2 * x))) * D(2) - (mortality_rate - (i ./ (i + 2 * x))) * D(3)]; % dR/dt

% Realistic Scenario: Increased Temperatures: slightly higher plant
% mortality, increased infectivity (up to a point, then decreased), higher
% fecundity, decreased recovery 
if i >=  x/2
    beta_adjust =  -0.25 * (i-25); 
else 
    beta_adjust =  0.25 * i;
end 

dP3dt =  @(t,D)[
(birth_rate + 0.1*i) - (beta + beta_adjust) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - (mortality_rate + 0.005*i) * D(1); % dS/dt
(beta + beta_adjust) * D(1) * (D(2)) / (D(1)+D(2)+D(3)) - ((mortality_rate +disease_mortality + 0.005*i)+(gamma - 0.005 * i))*D(2); % dI/dt
(gamma - 0.005 * i) * D(2) - (mortality_rate + 0.005*i) * D(3)]; % dR/dt


% Select which set of differential equations to use 
[t,plants] = ode15s(dP3dt, tspan, ic);

% Adds annual data + fall, winter, spring stasis into storage arrays: 
susceptible_stored = cat(1, susceptible_stored, plants(:,1), transpose(repelem(plants(end, 1), size(plants(:, 1), 1) * 3))); 
infected_stored = cat(1, infected_stored, plants(:,2), transpose(repelem(plants(end, 2), size(plants(:, 2), 1) * 3))); 
recovered_stored = cat(1, recovered_stored, plants(:,3), transpose(repelem(plants(end, 3), size(plants(:, 3), 1) * 3))); 

% Update Population Numbers for Start of next season 
susceptible = plants(end,1) + (5/8) * plants(end,3);
infected = plants(end,2);
recovered = (3/8) * plants(end,3);

end

% Visualizing Results: 
f1 = figure;
f2 = figure;
figure(f1);
plot(transpose(1:size(susceptible_stored, 1)) / (size(susceptible_stored, 1) / x), susceptible_stored(:,1), 'b', 'DisplayName', 'Susceptible'), hold on
plot(transpose(1:size(infected_stored, 1)) / (size(infected_stored, 1) / x), infected_stored(:,1), 'k', 'DisplayName', 'Infected')
plot(transpose(1:size(recovered_stored, 1)) / (size(recovered_stored, 1) / x), recovered_stored(:,1), 'r', 'DisplayName', 'Recovered'), grid on
legend('FontSize',14);
xlabel('Time (years)', 'FontSize',14)
ylabel('# Plants', 'FontSize',14)
ylim([0,100])
title('Tracking SIR Dynamics', 'FontSize',24);

figure(f2);
plot(transpose(1:size(infected_stored, 1)) / (size(infected_stored, 1) / x), infected_stored(:,1) + susceptible_stored(:,1) + recovered_stored(:,1), 'k', 'LineWidth',2, 'DisplayName', 'All Plants')
xlabel('Time (years)', 'FontSize',14)
ylabel('# Plants', 'FontSize',14)
ylim([0,100])
title('Total # Plants', 'FontSize',24);


