clear all
recycleTargets = 'yes'; %options: yes (E1, E2), no (E3, E4)
mixedTargetTalker = 'both'; %options: both (E1, E3), one (E2, E4)

%% SETUP
% We will choose the set of distractors based on whether we are recycling
% targets
if strcmp(recycleTargets,'yes')==1
    mBallDistractors = ["m150cave.wav";"m150tile.wav";"m150done.wav";"m150bluff.wav";"m150cad.wav";"m150cling.wav";"m150depth.wav";"m150dime.wav";"m150gnash.wav";"m150greet.wav";"m150jaw.wav";"m150jolt.wav";"m150knife.wav";"m150lash.wav";"m150park.wav";"m150priest.wav";"m150reek.wav";"m150romp.wav"];
    mCaveDistractors = ["m150ball.wav";"m150tile.wav";"m150done.wav";"m150bluff.wav";"m150cad.wav";"m150cling.wav";"m150depth.wav";"m150dime.wav";"m150gnash.wav";"m150greet.wav";"m150jaw.wav";"m150jolt.wav";"m150knife.wav";"m150lash.wav";"m150park.wav";"m150priest.wav";"m150reek.wav";"m150romp.wav"];
    mTileDistractors = ["m150ball.wav";"m150cave.wav";"m150done.wav";"m150bluff.wav";"m150cad.wav";"m150cling.wav";"m150depth.wav";"m150dime.wav";"m150gnash.wav";"m150greet.wav";"m150jaw.wav";"m150jolt.wav";"m150knife.wav";"m150lash.wav";"m150park.wav";"m150priest.wav";"m150reek.wav";"m150romp.wav"];
    mDoneDistractors = ["m150ball.wav";"m150cave.wav";"m150tile.wav";"m150bluff.wav";"m150cad.wav";"m150cling.wav";"m150depth.wav";"m150dime.wav";"m150gnash.wav";"m150greet.wav";"m150jaw.wav";"m150jolt.wav";"m150knife.wav";"m150lash.wav";"m150park.wav";"m150priest.wav";"m150reek.wav";"m150romp.wav"];
    
    fBallDistractors = ["m160cave.wav";"m160tile.wav";"m160done.wav";"m160bluff.wav";"m160cad.wav";"m160cling.wav";"m160depth.wav";"m160dime.wav";"m160gnash.wav";"m160greet.wav";"m160jaw.wav";"m160jolt.wav";"m160knife.wav";"m160lash.wav";"m160park.wav";"m160priest.wav";"m160reek.wav";"m160romp.wav"];
    fCaveDistractors = ["m160ball.wav";"m160tile.wav";"m160done.wav";"m160bluff.wav";"m160cad.wav";"m160cling.wav";"m160depth.wav";"m160dime.wav";"m160gnash.wav";"m160greet.wav";"m160jaw.wav";"m160jolt.wav";"m160knife.wav";"m160lash.wav";"m160park.wav";"m160priest.wav";"m160reek.wav";"m160romp.wav"];
    fTileDistractors = ["m160ball.wav";"m160cave.wav";"m160done.wav";"m160bluff.wav";"m160cad.wav";"m160cling.wav";"m160depth.wav";"m160dime.wav";"m160gnash.wav";"m160greet.wav";"m160jaw.wav";"m160jolt.wav";"m160knife.wav";"m160lash.wav";"m160park.wav";"m160priest.wav";"m160reek.wav";"m160romp.wav"];
    fDoneDistractors = ["m160ball.wav";"m160cave.wav";"m160tile.wav";"m160bluff.wav";"m160cad.wav";"m160cling.wav";"m160depth.wav";"m160dime.wav";"m160gnash.wav";"m160greet.wav";"m160jaw.wav";"m160jolt.wav";"m160knife.wav";"m160lash.wav";"m160park.wav";"m160priest.wav";"m160reek.wav";"m160romp.wav"];
    
elseif strcmp(recycleTargets, 'no') == 1
    mBallDistractors = ["m150bluff.wav";"m150cad.wav";"m150cling.wav";"m150depth.wav";"m150dime.wav";"m150gnash.wav";"m150greet.wav";"m150jaw.wav";"m150jolt.wav";"m150knife.wav";"m150lash.wav";"m150park.wav";"m150priest.wav";"m150reek.wav";"m150romp.wav"];
    mCaveDistractors = ["m150bluff.wav";"m150cad.wav";"m150cling.wav";"m150depth.wav";"m150dime.wav";"m150gnash.wav";"m150greet.wav";"m150jaw.wav";"m150jolt.wav";"m150knife.wav";"m150lash.wav";"m150park.wav";"m150priest.wav";"m150reek.wav";"m150romp.wav"];
    mTileDistractors = ["m150bluff.wav";"m150cad.wav";"m150cling.wav";"m150depth.wav";"m150dime.wav";"m150gnash.wav";"m150greet.wav";"m150jaw.wav";"m150jolt.wav";"m150knife.wav";"m150lash.wav";"m150park.wav";"m150priest.wav";"m150reek.wav";"m150romp.wav"];
    mDoneDistractors = ["m150bluff.wav";"m150cad.wav";"m150cling.wav";"m150depth.wav";"m150dime.wav";"m150gnash.wav";"m150greet.wav";"m150jaw.wav";"m150jolt.wav";"m150knife.wav";"m150lash.wav";"m150park.wav";"m150priest.wav";"m150reek.wav";"m150romp.wav"];
    
    fBallDistractors = ["m160bluff.wav";"m160cad.wav";"m160cling.wav";"m160depth.wav";"m160dime.wav";"m160gnash.wav";"m160greet.wav";"m160jaw.wav";"m160jolt.wav";"m160knife.wav";"m160lash.wav";"m160park.wav";"m160priest.wav";"m160reek.wav";"m160romp.wav"];
    fCaveDistractors = ["m160bluff.wav";"m160cad.wav";"m160cling.wav";"m160depth.wav";"m160dime.wav";"m160gnash.wav";"m160greet.wav";"m160jaw.wav";"m160jolt.wav";"m160knife.wav";"m160lash.wav";"m160park.wav";"m160priest.wav";"m160reek.wav";"m160romp.wav"];
    fTileDistractors = ["m160bluff.wav";"m160cad.wav";"m160cling.wav";"m160depth.wav";"m160dime.wav";"m160gnash.wav";"m160greet.wav";"m160jaw.wav";"m160jolt.wav";"m160knife.wav";"m160lash.wav";"m160park.wav";"m160priest.wav";"m160reek.wav";"m160romp.wav"];
    fDoneDistractors = ["m160bluff.wav";"m160cad.wav";"m160cling.wav";"m160depth.wav";"m160dime.wav";"m160gnash.wav";"m160greet.wav";"m160jaw.wav";"m160jolt.wav";"m160knife.wav";"m160lash.wav";"m160park.wav";"m160priest.wav";"m160reek.wav";"m160romp.wav"];
end
    
    %% MIXED TRIALS
    
    %First, figure out which positions will contain targets
    %Targets can only appear in positions 2-15 and must be separated by at
    %least one distractor
    
    targets = zeros(96*2,4);
    for i = 1:48
        targets(i,:) = randsample(2:15,4); targets(i,:) = sort(targets(i,:));
        while targets(i,4)-1 == targets(i,3) || targets(i,3)-1 == targets(i,2) || targets(i,2)-1 == targets(i,1)
            targets(i,:) = randsample(2:15,4); targets(i,:) = sort(targets(i,:));
        end
    end
    
    %Next, figure out which positions must have distractors (based on which ones
    %have targets)
    allPos = [1:16];
    for i = 1:48
        distractors(i,:) = setdiff(allPos, targets(i,:));
    end
    
    %Create a matrix that will contain all the stimulus names
    stimuli = zeros(96,16); stimuli = string(stimuli);
    
    % And now we load stimuli
    % This will depend on whether both talkers produce the targets on a given
    % trial (each producing half)
    % or whether just one talker produces all the targets for a given trial
    % (and then distractors are used to make sure each talker produces the same
    % number of stimuli in a trial)
    if strcmp(mixedTargetTalker, 'both') == 1
        
        % trials with BALL target
        for i = 1:12
            mStim = randsample(targets(i,:),2);
            fStim = setdiff(targets(i,:), mStim);
            stimuli(i, mStim) = "m150ball.wav";
            stimuli(i, fStim) = "m160ball.wav";
            
            trialDistractors = [randsample(fBallDistractors,6, true);randsample(mBallDistractors,6, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
        % trials with CAVE target
        for i = 13:24
            mStim = randsample(targets(i,:),2);
            fStim = setdiff(targets(i,:), mStim);
            stimuli(i, mStim) = "m150cave.wav";
            stimuli(i, fStim) = "m160cave.wav";
            
            trialDistractors = [randsample(fCaveDistractors,6, true);randsample(mCaveDistractors,6, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
        
        % trials with TILE target
        for i = 25:36
            mStim = randsample(targets(i,:),2);
            fStim = setdiff(targets(i,:), mStim);
            stimuli(i, mStim) = "m150tile.wav";
            stimuli(i, fStim) = "m160tile.wav";
            
            trialDistractors = [randsample(fTileDistractors,6, true);randsample(mTileDistractors,6, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
        
        % trials with DONE target
        for i = 37:48
            mStim = randsample(targets(i,:),2);
            fStim = setdiff(targets(i,:), mStim);
            stimuli(i, mStim) = "m150done.wav";
            stimuli(i, fStim) = "m160done.wav";
            
            trialDistractors = [randsample(fDoneDistractors,6, true);randsample(mDoneDistractors,6, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
    elseif strcmp(mixedTargetTalker, 'one') == 1
        
        % trials with BALL target
        for i = 1:6
            mStim = targets(i,:);
            stimuli(i, mStim) = "m150ball.wav";
            trialDistractors = [randsample(fBallDistractors,8, true);randsample(mBallDistractors,4, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        for i = 7:12
            fStim = targets(i,:);
            stimuli(i, fStim) = "m160ball.wav";
            trialDistractors = [randsample(fBallDistractors,4, true);randsample(mBallDistractors,8, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
        % trials with CAVE target
        for i = 13:18
            mStim = targets(i,:);
            stimuli(i, mStim) = "m150cave.wav";
            trialDistractors = [randsample(fCaveDistractors,8, true);randsample(mCaveDistractors,4, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        for i = 19:24
            fStim = targets(i,:);
            stimuli(i, fStim) = "m160cave.wav";
            trialDistractors = [randsample(fCaveDistractors,4, true);randsample(mCaveDistractors,8, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
        % trials with TILE target
        for i = 25:30
            mStim = targets(i,:);
            stimuli(i, mStim) = "m150tile.wav";
            trialDistractors = [randsample(fTileDistractors,8, true);randsample(mTileDistractors,4, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        for i = 31:36
            fStim = targets(i,:);
            stimuli(i, fStim) = "m160tile.wav";
            trialDistractors = [randsample(fTileDistractors,4, true);randsample(mTileDistractors,8, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        
        % trials with DONE target
        for i = 37:42
            mStim = targets(i,:);
            stimuli(i, mStim) = "m150done.wav";
            trialDistractors = [randsample(fDoneDistractors,8, true);randsample(mDoneDistractors,4, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
        for i = 43:48
            fStim = targets(i,:);
            stimuli(i, fStim) = "m160done.wav";
            trialDistractors = [randsample(fDoneDistractors,4, true);randsample(mDoneDistractors,8, true)];
            stimuli(i, distractors(i,:)) = randsample(trialDistractors,12);
        end
    end
    
    %% BLOCKED TRIALS: MALE
    %First, figure out which positions will contain targets
    %Targets can only appear in positions 2-15 and must be separated by at
    %least one distractor
    
    for i = 1:24
        targets(i,:) = randsample(2:15,4); targets(i,:) = sort(targets(i,:));
        while targets(i,4)-1 == targets(i,3) || targets(i,3)-1 == targets(i,2) || targets(i,2)-1 == targets(i,1)
            targets(i,:) = randsample(2:15,4); targets(i,:) = sort(targets(i,:));
        end
    end
    
    %Next, figure out which positions must have distractors
    for i = 1:24
        distractors(i,:) = setdiff(allPos, targets(i,:));
    end
    
    % trials with BALL target
    for i = 1:6
        stimuli(48+i, targets(i,:)) = "m150ball.wav";
        stimuli(48+i, distractors(i,:)) = randsample(mBallDistractors,12, true);
    end
    
    % trials with CAVE target
    for i = 7:12
        stimuli(48+i, targets(i,:)) = "m150cave.wav";
        stimuli(48+i, distractors(i,:)) = randsample(mCaveDistractors,12, true);
    end
    
    % trials with TILE target
    for i = 13:18
        stimuli(48+i, targets(i,:)) = "m150tile.wav";
        stimuli(48+i, distractors(i,:)) = randsample(mTileDistractors,12, true);
    end
    
    % trials with DONE target
    for i = 19:24
        stimuli(48+i, targets(i,:)) = "m150done.wav";
        stimuli(48+i, distractors(i,:)) = randsample(mDoneDistractors,12, true);
    end
    
    %% BLOCKED TRIALS: FEMALE
    %First, figure out which positions will contain targets
    %Targets can only appear in positions 2-15 and must be separated by at
    %least one distractor
    
    for i = 1:24
        targets(i,:) = randsample(2:15,4); targets(i,:) = sort(targets(i,:));
        while targets(i,4)-1 == targets(i,3) || targets(i,3)-1 == targets(i,2) || targets(i,2)-1 == targets(i,1)
            targets(i,:) = randsample(2:15,4); targets(i,:) = sort(targets(i,:));
        end
    end
    
    %Next, figure out which positions must have distractors
    for i = 1:24
        distractors(i,:) = setdiff(allPos, targets(i,:));
    end
    
    % trials with BALL target
    for i = 1:6
        stimuli(72+i, targets(i,:)) = "m160ball.wav";
        stimuli(72+i, distractors(i,:)) = randsample(fBallDistractors,12, true);
    end
    
    % trials with CAVE target
    for i = 7:12
        stimuli(72+i, targets(i,:)) = "m160cave.wav";
        stimuli(72+i, distractors(i,:)) = randsample(fCaveDistractors,12, true);
    end
    
    % trials with TILE target
    for i = 13:18
        stimuli(72+i, targets(i,:)) = "m160tile.wav";
        stimuli(72+i, distractors(i,:)) = randsample(fTileDistractors,12, true);
    end
    
    % trials with DONE target
    for i = 19:24
        stimuli(72+i, targets(i,:)) = "m160done.wav";
        stimuli(72+i, distractors(i,:)) = randsample(fDoneDistractors,12, true);
    end
