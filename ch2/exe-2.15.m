% Define the range for x and y with new limits
x = linspace(1, 2, 1000); % x from -2 to 2
y = linspace(1, 2, 1000);  % y from 3 to 5

function result = add_interval(interval1, interval2)
    result = [interval1(1) + interval2(1), interval1(2) + interval2(2)];
end

function result = mul_interval(interval1, interval2)
    a = interval1(1); b = interval1(2);
    c = interval2(1); d = interval2(2);
    products = [a*c, a*d, b*c, b*d];
    result = [min(products), max(products)];
end

function result = div_interval(interval1, interval2)
    a = interval1(1); b = interval1(2);
    inv_interval2 = [1/interval2(2), 1/interval2(1)]; % Inverse interval for division
    result = mul_interval(interval1, inv_interval2);
end


% Create a meshgrid for x and y
[X, Y] = meshgrid(x, y);

% Replace zeros in X and Y with a very small number (approaching zero as a limit)
epsilon = 1e-6; % Small number to approximate zero
X(X == 0) = epsilon;

% Calculate f(x, y)
F = 1 ./ (1./X + 1./Y);

% Find the minimum and maximum of F
[minValue, minIndex] = min(F(:));
[maxValue, maxIndex] = max(F(:));

% Get the coordinates of the minimum and maximum
[minX, minY] = ind2sub(size(F), minIndex);
[maxX, maxY] = ind2sub(size(F), maxIndex);

% Plot the function using surf
h = surf(X, Y, F);
hold on; % Keep the current plot

% Plot the minimum and maximum points
plot3(X(minX, minY), Y(minX, minY), minValue, 'go', 'MarkerSize', 2, 'MarkerFaceColor', 'g'); % Min as green dot
plot3(X(maxX, maxY), Y(maxX, maxY), maxValue, 'ro', 'MarkerSize', 2, 'MarkerFaceColor', 'r'); % Max as red dot

hold off; % Release the plot

% Add titles and labels
x_range_str = ['[', num2str(min(x)), ', ', num2str(max(x)), ']'];
y_range_str = ['[', num2str(min(y)), ', ', num2str(max(y)), ']'];
xlabel('x');
ylabel('y');
zlabel('f(x, y)');


% ... [Your existing code for plotting F goes here] ...

% Define your intervals (using X and Y ranges)
a = min(x); b = max(x); % Interval for x
c = min(y); d = max(y); % Interval for y

% Compute the resulting interval
interval_mul = mul_interval([a, b], [c, d]);
interval_add = add_interval([a, b], [c, d]);
disp(['interval_mul = [', num2str(interval_mul(1)), ', ', num2str(interval_mul(2)), ']']);
disp(['interval_add = [', num2str(interval_add(1)), ', ', num2str(interval_add(2)), ']']);
resulting_interval = div_interval(interval_mul, interval_add);
disp(['resulting_interval = [', num2str(resulting_interval(1)), ', ', num2str(resulting_interval(2)), ']']);

% The z-values for the lower and upper bounds of the resulting interval
z_lower_bound = resulting_interval(1);
z_upper_bound = resulting_interval(2);

% Create a grid that covers the entire x-y range for the planes
[X_plane, Y_plane] = meshgrid(linspace(min(x), max(x), 100), linspace(min(y), max(y), 100));

% Create Z matrices for the lower and upper bounds planes
Z_lower = z_lower_bound * ones(size(X_plane));
Z_upper = z_upper_bound * ones(size(X_plane));

% Plot the two horizontal planes representing the interval on the existing plot
hold on; % Keep the current plot
surf(X_plane, Y_plane, Z_lower, 'FaceColor', 'm', 'EdgeColor', 'none', 'FaceAlpha', 0.3); % Lower bound plane
surf(X_plane, Y_plane, Z_upper, 'FaceColor', 'm', 'EdgeColor', 'none', 'FaceAlpha', 0.3); % Upper bound plane

% Add annotations for the planes if necessary
text(max(x), max(y), z_lower_bound, ['Lower bound (par1): ', num2str(z_lower_bound)], 'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right');
text(max(x), max(y), z_upper_bound, ['Upper bound (par1): ', num2str(z_upper_bound)], 'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right');

par1_range_str = ['[', num2str(resulting_interval(1)), ', ', num2str(resulting_interval(2)), ']']




% Define your intervals (using X and Y ranges)
a = min(x); b = max(x); % Interval for x
c = min(y); d = max(y); % Interval for y

% Compute the resulting interval



resulting_interval_par2 = div_interval([1,1], add_interval(div_interval([1,1], [a,b]), div_interval([1,1], [c,d])));
disp(['resulting_interval_par2 = [', num2str(resulting_interval_par2(1)), ', ', num2str(resulting_interval_par2(2)), ']']);

% The z-values for the lower and upper bounds of the resulting interval
z_lower_bound_par2 = resulting_interval_par2(1);
z_upper_bound_par2 = resulting_interval_par2(2);

[X_plane, Y_plane] = meshgrid(linspace(min(x), max(x), 100), linspace(min(y), max(y), 100));

% Create Z matrices for the lower and upper bounds planes
Z_lower_par2 = z_lower_bound_par2 * ones(size(X_plane));
Z_upper_par2 = z_upper_bound_par2 * ones(size(X_plane));

% Plot the two horizontal planes representing the interval on the existing plot
hold on; % Keep the current plot
surf(X_plane, Y_plane, Z_lower_par2, 'FaceColor', 'c', 'EdgeColor', 'none'); % Lower bound plane
surf(X_plane, Y_plane, Z_upper_par2, 'FaceColor', 'c', 'EdgeColor', 'none'); % Upper bound plane



% Add annotations for the planes if necessary
text(max(x), max(y), z_lower_bound_par2, ['Lower bound (par2): ', num2str(z_lower_bound_par2)], 'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right');
text(max(x), max(y), z_upper_bound_par2, ['Upper bound (par2): ', num2str(z_upper_bound_par2)], 'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right');

par2_range_str = ['[', num2str(resulting_interval_par2(1)), ', ', num2str(resulting_interval_par2(2)), ']']

title(['Interactive Surface plot of f(x, y) = 1 / (1/x + 1/y) for x = ', x_range_str, ' and y = ', y_range_str, 'and the plot of par2 for R1 = ', x_range_str, ' and R2 = ', y_range_str, ' and par1(x,y) = ', par1_range_str , ' par2(x,y) = ' , par2_range_str ]);

% ... [Rest of your existing code for plot settings and enhancements] ...

% Enhancements for smoothness and appearance
%shading interp; % Interpolated shading for smooth color transitions
%set(h, 'EdgeColor', 'none'); % Remove grid lines to enhance smoothness

% Enable rotation
rotate3d on;
%print -dpng -r300 "exe-2.15-plot-diff-par1-par2.png"

