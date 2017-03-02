// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// back_test
Rcpp::List back_test(Rcpp::LogicalVector enter, Rcpp::LogicalVector exit, Rcpp::NumericVector price, double stop_loss, int side);
RcppExport SEXP QuantTools_back_test(SEXP enterSEXP, SEXP exitSEXP, SEXP priceSEXP, SEXP stop_lossSEXP, SEXP sideSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type enter(enterSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type exit(exitSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type price(priceSEXP);
    Rcpp::traits::input_parameter< double >::type stop_loss(stop_lossSEXP);
    Rcpp::traits::input_parameter< int >::type side(sideSEXP);
    rcpp_result_gen = Rcpp::wrap(back_test(enter, exit, price, stop_loss, side));
    return rcpp_result_gen;
END_RCPP
}
// bbands
Rcpp::List bbands(Rcpp::NumericVector x, std::size_t n, double k);
RcppExport SEXP QuantTools_bbands(SEXP xSEXP, SEXP nSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(bbands(x, n, k));
    return rcpp_result_gen;
END_RCPP
}
// crossover
Rcpp::IntegerVector crossover(Rcpp::NumericVector x, Rcpp::NumericVector y);
RcppExport SEXP QuantTools_crossover(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(crossover(x, y));
    return rcpp_result_gen;
END_RCPP
}
// ema
std::vector<double> ema(Rcpp::NumericVector x, std::size_t n);
RcppExport SEXP QuantTools_ema(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(ema(x, n));
    return rcpp_result_gen;
END_RCPP
}
// na_locf_numeric
Rcpp::NumericVector na_locf_numeric(Rcpp::NumericVector x);
RcppExport SEXP QuantTools_na_locf_numeric(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(na_locf_numeric(x));
    return rcpp_result_gen;
END_RCPP
}
// roll_lm
Rcpp::List roll_lm(Rcpp::NumericVector x, Rcpp::NumericVector y, std::size_t n);
RcppExport SEXP QuantTools_roll_lm(SEXP xSEXP, SEXP ySEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_lm(x, y, n));
    return rcpp_result_gen;
END_RCPP
}
// roll_correlation
std::vector< double > roll_correlation(Rcpp::NumericVector x, Rcpp::NumericVector y, std::size_t n);
RcppExport SEXP QuantTools_roll_correlation(SEXP xSEXP, SEXP ySEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_correlation(x, y, n));
    return rcpp_result_gen;
END_RCPP
}
// roll_percent_rank
std::vector<double> roll_percent_rank(Rcpp::NumericVector x, std::size_t n);
RcppExport SEXP QuantTools_roll_percent_rank(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_percent_rank(x, n));
    return rcpp_result_gen;
END_RCPP
}
// roll_range
Rcpp::List roll_range(Rcpp::NumericVector x, std::size_t n);
RcppExport SEXP QuantTools_roll_range(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_range(x, n));
    return rcpp_result_gen;
END_RCPP
}
// roll_quantile
std::vector<double> roll_quantile(Rcpp::NumericVector x, std::size_t n, double p);
RcppExport SEXP QuantTools_roll_quantile(SEXP xSEXP, SEXP nSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_quantile(x, n, p));
    return rcpp_result_gen;
END_RCPP
}
// roll_min
std::vector<double> roll_min(Rcpp::NumericVector x, std::size_t n);
RcppExport SEXP QuantTools_roll_min(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_min(x, n));
    return rcpp_result_gen;
END_RCPP
}
// roll_max
std::vector<double> roll_max(Rcpp::NumericVector x, std::size_t n);
RcppExport SEXP QuantTools_roll_max(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_max(x, n));
    return rcpp_result_gen;
END_RCPP
}
// roll_sd
std::vector<double> roll_sd(Rcpp::NumericVector x, std::size_t n);
RcppExport SEXP QuantTools_roll_sd(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_sd(x, n));
    return rcpp_result_gen;
END_RCPP
}
// roll_sd_filter
std::vector< bool > roll_sd_filter(Rcpp::NumericVector x, int n, double k, int m);
RcppExport SEXP QuantTools_roll_sd_filter(SEXP xSEXP, SEXP nSEXP, SEXP kSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type k(kSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_sd_filter(x, n, k, m));
    return rcpp_result_gen;
END_RCPP
}
// roll_volume_profile
Rcpp::List roll_volume_profile(Rcpp::DataFrame ticks, int timeFrame, double step, double alpha, double cut);
RcppExport SEXP QuantTools_roll_volume_profile(SEXP ticksSEXP, SEXP timeFrameSEXP, SEXP stepSEXP, SEXP alphaSEXP, SEXP cutSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type ticks(ticksSEXP);
    Rcpp::traits::input_parameter< int >::type timeFrame(timeFrameSEXP);
    Rcpp::traits::input_parameter< double >::type step(stepSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type cut(cutSEXP);
    rcpp_result_gen = Rcpp::wrap(roll_volume_profile(ticks, timeFrame, step, alpha, cut));
    return rcpp_result_gen;
END_RCPP
}
// rsi
std::vector<double> rsi(Rcpp::NumericVector x, std::size_t n);
RcppExport SEXP QuantTools_rsi(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(rsi(x, n));
    return rcpp_result_gen;
END_RCPP
}
// run_tests
bool run_tests();
RcppExport SEXP QuantTools_run_tests() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(run_tests());
    return rcpp_result_gen;
END_RCPP
}
// sma
std::vector<double> sma(Rcpp::NumericVector x, int n);
RcppExport SEXP QuantTools_sma(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(sma(x, n));
    return rcpp_result_gen;
END_RCPP
}
// stochastic
Rcpp::List stochastic(SEXP x, size_t n, size_t nFast, size_t nSlow);
RcppExport SEXP QuantTools_stochastic(SEXP xSEXP, SEXP nSEXP, SEXP nFastSEXP, SEXP nSlowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< size_t >::type n(nSEXP);
    Rcpp::traits::input_parameter< size_t >::type nFast(nFastSEXP);
    Rcpp::traits::input_parameter< size_t >::type nSlow(nSlowSEXP);
    rcpp_result_gen = Rcpp::wrap(stochastic(x, n, nFast, nSlow));
    return rcpp_result_gen;
END_RCPP
}
// to_candles
Rcpp::List to_candles(Rcpp::DataFrame ticks, int timeframe);
RcppExport SEXP QuantTools_to_candles(SEXP ticksSEXP, SEXP timeframeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type ticks(ticksSEXP);
    Rcpp::traits::input_parameter< int >::type timeframe(timeframeSEXP);
    rcpp_result_gen = Rcpp::wrap(to_candles(ticks, timeframe));
    return rcpp_result_gen;
END_RCPP
}
