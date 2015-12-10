 def run_train_routine(self, gridsearch, carrier=None, method='validation'):
        if carrier == None:
            carrier = self.carrier

        # Run xgboost training routine, put in cross validation later


        grid = [(tree, depth, eta, subsample, colsample, minchildweight) for tree in gridsearch['tree'] for depth in
                gridsearch['depth'] \
                for eta in gridsearch['eta'] for subsample in gridsearch['subsample'] for colsample in
                gridsearch['colsample'] for minchildweight in gridsearch['minchild']]

        if method == 'validation':
            # Create validation set, use same split percentages for simplicity
            self.X_train, self.X_valid, self.y_train, self.y_valid = train_test_split(self.X_train, self.y_train,
                                                                                      test_size=self.test_size,
                                                                                      train_size=self.train_size)
            dtrain = xgb.DMatrix(self.X_train, label=self.y_train)
            dtest = xgb.DMatrix(self.X_test, label=self.y_test)
            dvalid = xgb.DMatrix(self.X_valid, label=self.y_valid)

            validation_results = []
            for i in grid:
                param = {'bst:max_depth': i[1], 'bst:eta': i[2], 'subsample': i[3], 'silent': 0, 'bst:gamma': 0.0,
                         'objective': 'reg:linear', 'booster': 'gbtree', 'colsample_bytree': i[4],
                         'min_child_weight': i[5], 'eval_metric': 'rmse'}

                param['nthread'] = 8

                evallist = [(dvalid, 'eval'), (dtrain, 'train')]

                num_round = i[0]

                bst = xgb.train(param, dtrain, num_round)

                ypred = bst.predict(dvalid)

                try:
                    score = r2_score(exp(self.y_valid), exp(ypred))
                except ValueError:
                    score = 0

                validation_results.append([i[y] for y in [0, 1, 2, 3, 4, 5]] + [np.round(score, 2)])

            validation = pd.DataFrame(validation_results,
                                      columns=['trees', 'max_depth', 'eta', 'subsample', 'colsample',
                                               'min_child_weight', 'score'])
            print validation
            validation['opp_trees'] = validation[
                                          'trees'] ** -1  # we want the best cross validation score with the smallest amt of trees
            validation.sort(['score', 'opp_trees'], ascending=False, inplace=True)
            optimal_params = validation.iloc[0].to_dict()

            # Now that we've tuned, retrain on train+test
            dtrain = xgb.DMatrix(np.concatenate((self.X_train, self.X_valid)),
                                 label=np.concatenate((self.y_train, self.y_valid)))

            dtest = xgb.DMatrix(self.X_test, label=self.y_test)

            param = {'bst:max_depth': optimal_params['max_depth'], 'bst:eta': optimal_params['eta'],
                     'subsample': optimal_params['subsample'],
                     'silent': 1, 'bst:gamma': 0.0, 'objective': 'reg:linear',
                     'booster': 'gbtree', 'colsample_bytree': optimal_params['colsample'],
                     'min_child_weight': optimal_params['min_child_weight'], 'eval_metric': 'rmse'}

            param['nthread'] = 8

            evallist = [(dvalid, 'eval'), (dtrain, 'train')]

            num_round = int(optimal_params['trees'])

            bst = xgb.train(param, dtrain, num_round)

            # Predict final score on test set, do nothing else after this

            ypred = bst.predict(dtest)

            score = r2_score(exp(self.y_test), exp(ypred))

            self.final_score = score

            print str(score) + ' is the final r squared score on the test set'

            # Train on full set on optimal params and dump model, using above params

            dtrain = xgb.DMatrix(np.concatenate((self.X_train, self.X_test)),
                                 label=np.concatenate((self.y_train, self.y_test)))
            print "Training final model"
            self.bst = xgb.train(param, dtrain, num_round)
            print "Finished training full model. Go have a snack"

        if method == 'cv':
            return None
            # USE SCIKIT K FOLD