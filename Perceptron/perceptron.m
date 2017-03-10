%% Find the linear separator w*

% Read the data labeled '0' and '1'
load('mnist_all.mat', 'test0', 'test1', 'train0', 'train1');

% Append train0 and train1 (Create column 785 for 0 and 1)
train0_num = size(train0, 1);
train1_num = size(train1, 1);
train_ = [train0 -ones(train0_num, 1); train1 ones(train1_num, 1)];
% Transfer the format from uint8 to double (and normalization)
train = im2double(train_(:, 1:784));
% Initial w(0)
w = zeros(1, 784);
t = 0;
mistake = 1;
while mistake == 1
    mistake = 0;
    % Inner product of matrix 'train' and vector 'w'
    q = train * w';
    % Times the sign (for train0, y = -1)
    q(1:5923, :) = q(1:5923, :) * -1;
    q1 = 0;
    % Randomly pick a row (a) which is less than or equal to 0
    while q1 == 0
        a = randi([1 12665], 1, 1);
        q1 = q(a, 1) <= 0;
        % b ~= 0 means there is at least 1 row which is less than or equal to 0
        b = sum((q <= 0)' .* ones(1, 12665)); 
        % If all rows are larger than 0, stop and return w(t)
        if b == 0
            q1 = 1;
        end
    end
    % Update w(t+1) by adding y_a*x_a (a is randomly selected)
    if q1 == 1 && b ~=0
        if a <= 5923
            w = w - train(a, 1:784);
        else
            w = w + train(a, 1:784);
        end
        mistake = 1;
        t = t + 1;
    end
end

%%
% I first append 2 matrix, which has transfer the format and normalize, and
% create a larger matrix with 784 columns, which is variable _*train*_.
% Second step is to create a zero vector with size $1\times784$, which is
% _*w*_.

%%
% Then I start to run the perceptron, in which I use |while| statement to
% test whether all the inner product equal to 0.
% While running perceptron, I randomly pick the beginning point, which
% makes the result ($w^*$ and iteration) be different everytime I run the
% perceptron.


%% Test the Accuracy

% Transfer the format from uint8 to double (and normalize)
test_0 = im2double(test0);
test_1 = im2double(test1);
% Check the sign of test0 and test1
q_0 = -1 * test_0 * w';
q_1 = test_1 * w';
% Compute the number which x <= 0 (opposite sign)
n_0 = size(q_0(q_0 <= 0), 1);
n_1 = size(q_1(q_1 <= 0), 1);
% Accuracy rate
acc = ((size(q_0, 1) + size(q_1, 1)) - (n_0 + n_1))/(size(q_0, 1) + size(q_1, 1));

%%
% I use |while| command to run the perception, which means the code stops 
% when every signs are consistent. As a result, the accuracy is 100% in 
% _*train0*_ and _*train1*_.
%%
% Double check by using file _*test0*_ and _*test1*_, the accuracy rate is
% over 99%. Since the beginning point is randomly selected, there is no
% certain accuracy rate; however, the accuracy rate always greater than 99%.
%% Feature Reduction

re_w = w;
re_train = train;
re_test_0 = test_0;
re_test_1 = test_1;
re_acc = acc;
re = 0;
% Feature reduction, reduce features which has little effects
% Eliminate the column whenever it equals to 0 in w, until there is no 0
while size(find(abs(re_w) <= 1, 1)', 1) ~= 0  
    % Find out which column in 're_w' equals to 0
    w_ = find(abs(re_w) <= 1);    
    % Eliminate the corresponding column in 're_train'
    re_train(:, w_) = [];
    % Use 's' to obtain new size after reducing
    s = size(re_train', 1);
    re_w = zeros(1, s);
    re_t = 0;
    mistake = 1;
    while mistake == 1
        mistake = 0;
        re_q = re_train * re_w';
        re_q(1:5923, :) = re_q(1:5923, :) * -1;
        q1 = 0;
        while q1 == 0
            a = randi([1 12665], 1, 1);
            q1 = re_q(a, 1) <= 0;
            b = sum((re_q <= 0)' .* ones(1, 12665)); 
            if b == 0
                q1 = 1;
            end
        end
        if q1 == 1 && b ~=0
            if a <= 5923
                re_w = re_w - re_train(a, 1:s);
            else
                re_w = re_w + re_train(a, 1:s);
            end
            mistake = 1;
            re_t = re_t + 1;
        end
    end
    re_test_0(:, w_) = [];
    re_test_1(:, w_) = [];
    re_q_0 = -1 * re_test_0 * re_w';
    re_q_1 = re_test_1 * re_w';
    re_n_0 = size(re_q_0(re_q_0 <= 0), 1);
    re_n_1 = size(re_q_1(re_q_1 <= 0), 1);
    re_acc = ((size(re_q_0, 1) + size(re_q_1, 1)) - (re_n_0 + re_n_1)) ...
             /(size(re_q_0, 1) + size(re_q_1, 1));
    % Record how many times I run the feature reduction
    re = re + 1;
end

%%
% When doing the feature reduction, I locate the column which absolute value
% less or equals to 0.5 %in _*re_w*_, then eliminate the corresponding
% column in _*re_train*_. I set 0.5 as threshold since the accuracy rate
% decreases when the features are reduced.
% I delete about 640 features. Since the feature I reduce are corresponding
% to the column which is smaller than 0.2 in boundary, which means they
% have trivial effect on the perceptron.

%%
% I do the reduction for multiple times and make sure the remaining
% features will all correspond to the column which is greater than 0.5 in
% the classifier. The columns in classifier _*re_w*_ means the features I
% keep will have certain effect on the perceptron.

%%
% I re-run the accuracy rate check after eliminating all the possible 0
% columns, the accuracy rate doesn't diminish much and still greater than
% 99%.


