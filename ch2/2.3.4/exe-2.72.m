% Define the range of i
i = 1:1000;

% Calculate the probability distribution values
probabilities = 1 ./ (2 .^ i);

% Plot the probability distribution
figure;
plot(i, probabilities, 'LineWidth', 2);
title('Probability Distribution of 1/2^i');
xlabel('i');
ylabel('Probability');
grid on;
