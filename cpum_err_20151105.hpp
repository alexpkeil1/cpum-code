// Code generated by Stan version 2.8

#include <stan/model/model_header.hpp>

namespace cpum_err_20151105_model_namespace {

using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;

typedef Eigen::Matrix<double,Eigen::Dynamic,1> vector_d;
typedef Eigen::Matrix<double,1,Eigen::Dynamic> row_vector_d;
typedef Eigen::Matrix<double,Eigen::Dynamic,Eigen::Dynamic> matrix_d;

static int current_statement_begin__;
class cpum_err_20151105_model : public prob_grad {
private:
    int N;
    int J;
    int minT;
    int maxT;
    int obscomplete;
    row_vector_d id;
    row_vector_d wlm;
    row_vector_d cumwlm;
    row_vector_d cumwlm2lag;
    row_vector_d atwork;
    row_vector_d atwork2lag;
    row_vector_d agein;
    row_vector_d ageout;
    row_vector_d dateout;
    row_vector_d cohort_1;
    row_vector_d BL_cumwlm;
    row_vector_d BL_cumyrsexp;
    row_vector_d cumyrsexp;
    vector<int> leftwork;
    vector<int> d_lc;
    vector<int> d_nonlc;
    row_vector_d ageoutcen;
    row_vector_d dateoutcen;
    row_vector_d py;
    row_vector_d BL_cumwlmcen;
    row_vector_d BL_cumyrsexpcen;
    row_vector_d cumyrsexpcen;
    row_vector_d cumwlm2lagcen;
    double meanageout;
    double sdageout;
    double meandateout;
    double sddateout;
    double meanBL_cumwlm;
    double meanBL_cumyrsexp;
    double meancumyrsexp;
    double meancumwlm2lag;
    double sdBL_cumwlm;
    double sdBL_cumyrsexp;
    double sdcumyrsexp;
    double sdcumwlm2lag;
    double maxX;
public:
    cpum_err_20151105_model(stan::io::var_context& context__,
        std::ostream* pstream__ = 0)
        : prob_grad(0) {
        current_statement_begin__ = -1;

        static const char* function__ = "cpum_err_20151105_model_namespace::cpum_err_20151105_model";
        (void) function__; // dummy call to supress warning
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<int> vals_i__;
        std::vector<double> vals_r__;
        context__.validate_dims("data initialization", "N", "int", context__.to_vec());
        N = int(0);
        vals_i__ = context__.vals_i("N");
        pos__ = 0;
        N = vals_i__[pos__++];
        context__.validate_dims("data initialization", "J", "int", context__.to_vec());
        J = int(0);
        vals_i__ = context__.vals_i("J");
        pos__ = 0;
        J = vals_i__[pos__++];
        context__.validate_dims("data initialization", "minT", "int", context__.to_vec());
        minT = int(0);
        vals_i__ = context__.vals_i("minT");
        pos__ = 0;
        minT = vals_i__[pos__++];
        context__.validate_dims("data initialization", "maxT", "int", context__.to_vec());
        maxT = int(0);
        vals_i__ = context__.vals_i("maxT");
        pos__ = 0;
        maxT = vals_i__[pos__++];
        context__.validate_dims("data initialization", "obscomplete", "int", context__.to_vec());
        obscomplete = int(0);
        vals_i__ = context__.vals_i("obscomplete");
        pos__ = 0;
        obscomplete = vals_i__[pos__++];
        context__.validate_dims("data initialization", "id", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("id", "obscomplete", obscomplete);
        id = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("id");
        pos__ = 0;
        size_t id_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < id_i_vec_lim__; ++i_vec__) {
            id[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "wlm", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("wlm", "obscomplete", obscomplete);
        wlm = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("wlm");
        pos__ = 0;
        size_t wlm_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < wlm_i_vec_lim__; ++i_vec__) {
            wlm[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "cumwlm", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("cumwlm", "obscomplete", obscomplete);
        cumwlm = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("cumwlm");
        pos__ = 0;
        size_t cumwlm_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < cumwlm_i_vec_lim__; ++i_vec__) {
            cumwlm[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "cumwlm2lag", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("cumwlm2lag", "obscomplete", obscomplete);
        cumwlm2lag = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("cumwlm2lag");
        pos__ = 0;
        size_t cumwlm2lag_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < cumwlm2lag_i_vec_lim__; ++i_vec__) {
            cumwlm2lag[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "atwork", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("atwork", "obscomplete", obscomplete);
        atwork = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("atwork");
        pos__ = 0;
        size_t atwork_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < atwork_i_vec_lim__; ++i_vec__) {
            atwork[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "atwork2lag", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("atwork2lag", "obscomplete", obscomplete);
        atwork2lag = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("atwork2lag");
        pos__ = 0;
        size_t atwork2lag_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < atwork2lag_i_vec_lim__; ++i_vec__) {
            atwork2lag[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "agein", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("agein", "obscomplete", obscomplete);
        agein = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("agein");
        pos__ = 0;
        size_t agein_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < agein_i_vec_lim__; ++i_vec__) {
            agein[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "ageout", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("ageout", "obscomplete", obscomplete);
        ageout = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("ageout");
        pos__ = 0;
        size_t ageout_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < ageout_i_vec_lim__; ++i_vec__) {
            ageout[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "dateout", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("dateout", "obscomplete", obscomplete);
        dateout = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("dateout");
        pos__ = 0;
        size_t dateout_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < dateout_i_vec_lim__; ++i_vec__) {
            dateout[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "cohort_1", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("cohort_1", "obscomplete", obscomplete);
        cohort_1 = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("cohort_1");
        pos__ = 0;
        size_t cohort_1_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < cohort_1_i_vec_lim__; ++i_vec__) {
            cohort_1[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "BL_cumwlm", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("BL_cumwlm", "obscomplete", obscomplete);
        BL_cumwlm = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("BL_cumwlm");
        pos__ = 0;
        size_t BL_cumwlm_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < BL_cumwlm_i_vec_lim__; ++i_vec__) {
            BL_cumwlm[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "BL_cumyrsexp", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("BL_cumyrsexp", "obscomplete", obscomplete);
        BL_cumyrsexp = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("BL_cumyrsexp");
        pos__ = 0;
        size_t BL_cumyrsexp_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < BL_cumyrsexp_i_vec_lim__; ++i_vec__) {
            BL_cumyrsexp[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "cumyrsexp", "row_vector_d", context__.to_vec(obscomplete));
        validate_non_negative_index("cumyrsexp", "obscomplete", obscomplete);
        cumyrsexp = row_vector_d(obscomplete);
        vals_r__ = context__.vals_r("cumyrsexp");
        pos__ = 0;
        size_t cumyrsexp_i_vec_lim__ = obscomplete;
        for (size_t i_vec__ = 0; i_vec__ < cumyrsexp_i_vec_lim__; ++i_vec__) {
            cumyrsexp[i_vec__] = vals_r__[pos__++];
        }
        context__.validate_dims("data initialization", "leftwork", "int", context__.to_vec(obscomplete));
        validate_non_negative_index("leftwork", "obscomplete", obscomplete);
        leftwork = std::vector<int>(obscomplete,int(0));
        vals_i__ = context__.vals_i("leftwork");
        pos__ = 0;
        size_t leftwork_limit_0__ = obscomplete;
        for (size_t i_0__ = 0; i_0__ < leftwork_limit_0__; ++i_0__) {
            leftwork[i_0__] = vals_i__[pos__++];
        }
        context__.validate_dims("data initialization", "d_lc", "int", context__.to_vec(obscomplete));
        validate_non_negative_index("d_lc", "obscomplete", obscomplete);
        d_lc = std::vector<int>(obscomplete,int(0));
        vals_i__ = context__.vals_i("d_lc");
        pos__ = 0;
        size_t d_lc_limit_0__ = obscomplete;
        for (size_t i_0__ = 0; i_0__ < d_lc_limit_0__; ++i_0__) {
            d_lc[i_0__] = vals_i__[pos__++];
        }
        context__.validate_dims("data initialization", "d_nonlc", "int", context__.to_vec(obscomplete));
        validate_non_negative_index("d_nonlc", "obscomplete", obscomplete);
        d_nonlc = std::vector<int>(obscomplete,int(0));
        vals_i__ = context__.vals_i("d_nonlc");
        pos__ = 0;
        size_t d_nonlc_limit_0__ = obscomplete;
        for (size_t i_0__ = 0; i_0__ < d_nonlc_limit_0__; ++i_0__) {
            d_nonlc[i_0__] = vals_i__[pos__++];
        }

        // validate data
        check_greater_or_equal(function__,"N",N,0);
        check_greater_or_equal(function__,"J",J,0);
        check_greater_or_equal(function__,"minT",minT,0);
        check_greater_or_equal(function__,"maxT",maxT,0);
        check_greater_or_equal(function__,"obscomplete",obscomplete,0);
        for (int k0__ = 0; k0__ < obscomplete; ++k0__) {
            check_greater_or_equal(function__,"leftwork[k0__]",leftwork[k0__],0);
            check_less_or_equal(function__,"leftwork[k0__]",leftwork[k0__],1);
        }
        for (int k0__ = 0; k0__ < obscomplete; ++k0__) {
            check_greater_or_equal(function__,"d_lc[k0__]",d_lc[k0__],0);
            check_less_or_equal(function__,"d_lc[k0__]",d_lc[k0__],1);
        }
        for (int k0__ = 0; k0__ < obscomplete; ++k0__) {
            check_greater_or_equal(function__,"d_nonlc[k0__]",d_nonlc[k0__],0);
            check_less_or_equal(function__,"d_nonlc[k0__]",d_nonlc[k0__],1);
        }
        validate_non_negative_index("ageoutcen", "obscomplete", obscomplete);
        ageoutcen = row_vector_d(obscomplete);
        validate_non_negative_index("dateoutcen", "obscomplete", obscomplete);
        dateoutcen = row_vector_d(obscomplete);
        validate_non_negative_index("py", "obscomplete", obscomplete);
        py = row_vector_d(obscomplete);
        validate_non_negative_index("BL_cumwlmcen", "obscomplete", obscomplete);
        BL_cumwlmcen = row_vector_d(obscomplete);
        validate_non_negative_index("BL_cumyrsexpcen", "obscomplete", obscomplete);
        BL_cumyrsexpcen = row_vector_d(obscomplete);
        validate_non_negative_index("cumyrsexpcen", "obscomplete", obscomplete);
        cumyrsexpcen = row_vector_d(obscomplete);
        validate_non_negative_index("cumwlm2lagcen", "obscomplete", obscomplete);
        cumwlm2lagcen = row_vector_d(obscomplete);
        meanageout = double(0);
        sdageout = double(0);
        meandateout = double(0);
        sddateout = double(0);
        meanBL_cumwlm = double(0);
        meanBL_cumyrsexp = double(0);
        meancumyrsexp = double(0);
        meancumwlm2lag = double(0);
        sdBL_cumwlm = double(0);
        sdBL_cumyrsexp = double(0);
        sdcumyrsexp = double(0);
        sdcumwlm2lag = double(0);
        maxX = double(0);

        double DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning


        // initialize transformed variables to avoid seg fault on val access
        stan::math::fill(ageoutcen,DUMMY_VAR__);
        stan::math::fill(dateoutcen,DUMMY_VAR__);
        stan::math::fill(py,DUMMY_VAR__);
        stan::math::fill(BL_cumwlmcen,DUMMY_VAR__);
        stan::math::fill(BL_cumyrsexpcen,DUMMY_VAR__);
        stan::math::fill(cumyrsexpcen,DUMMY_VAR__);
        stan::math::fill(cumwlm2lagcen,DUMMY_VAR__);
        stan::math::fill(meanageout,DUMMY_VAR__);
        stan::math::fill(sdageout,DUMMY_VAR__);
        stan::math::fill(meandateout,DUMMY_VAR__);
        stan::math::fill(sddateout,DUMMY_VAR__);
        stan::math::fill(meanBL_cumwlm,DUMMY_VAR__);
        stan::math::fill(meanBL_cumyrsexp,DUMMY_VAR__);
        stan::math::fill(meancumyrsexp,DUMMY_VAR__);
        stan::math::fill(meancumwlm2lag,DUMMY_VAR__);
        stan::math::fill(sdBL_cumwlm,DUMMY_VAR__);
        stan::math::fill(sdBL_cumyrsexp,DUMMY_VAR__);
        stan::math::fill(sdcumyrsexp,DUMMY_VAR__);
        stan::math::fill(sdcumwlm2lag,DUMMY_VAR__);
        stan::math::fill(maxX,DUMMY_VAR__);

        try {
            current_statement_begin__ = 62;
            stan::math::assign(meanageout, mean(ageout));
            current_statement_begin__ = 63;
            stan::math::assign(meandateout, mean(dateout));
            current_statement_begin__ = 64;
            stan::math::assign(meanBL_cumwlm, mean(BL_cumwlm));
            current_statement_begin__ = 65;
            stan::math::assign(meanBL_cumyrsexp, mean(BL_cumyrsexp));
            current_statement_begin__ = 66;
            stan::math::assign(meancumyrsexp, mean(cumyrsexp));
            current_statement_begin__ = 67;
            stan::math::assign(meancumwlm2lag, mean(cumwlm2lag));
            current_statement_begin__ = 68;
            stan::math::assign(sdageout, sd(ageout));
            current_statement_begin__ = 69;
            stan::math::assign(sddateout, sd(dateout));
            current_statement_begin__ = 70;
            stan::math::assign(sdBL_cumwlm, sd(BL_cumwlm));
            current_statement_begin__ = 71;
            stan::math::assign(sdBL_cumyrsexp, sd(BL_cumyrsexp));
            current_statement_begin__ = 72;
            stan::math::assign(sdcumyrsexp, sd(cumyrsexp));
            current_statement_begin__ = 73;
            stan::math::assign(sdcumwlm2lag, sd(cumwlm2lag));
            current_statement_begin__ = 78;
            for (int n = 1; n <= obscomplete; ++n) {
                current_statement_begin__ = 79;
                stan::math::assign(get_base1_lhs(py,n,"py",1), ((get_base1(ageout,n,"ageout",1) - get_base1(agein,n,"agein",1)) + 0.00044910000000000002));
                current_statement_begin__ = 80;
                stan::math::assign(get_base1_lhs(ageoutcen,n,"ageoutcen",1), ((get_base1(ageout,n,"ageout",1) - meanageout) / sdageout));
                current_statement_begin__ = 81;
                stan::math::assign(get_base1_lhs(dateoutcen,n,"dateoutcen",1), ((get_base1(dateout,n,"dateout",1) - meandateout) / sddateout));
                current_statement_begin__ = 82;
                stan::math::assign(get_base1_lhs(BL_cumwlmcen,n,"BL_cumwlmcen",1), ((get_base1(BL_cumwlm,n,"BL_cumwlm",1) - meanBL_cumwlm) / sdBL_cumwlm));
                current_statement_begin__ = 83;
                stan::math::assign(get_base1_lhs(BL_cumyrsexpcen,n,"BL_cumyrsexpcen",1), ((get_base1(BL_cumyrsexp,n,"BL_cumyrsexp",1) - meanBL_cumyrsexp) / sdBL_cumyrsexp));
                current_statement_begin__ = 84;
                stan::math::assign(get_base1_lhs(cumyrsexpcen,n,"cumyrsexpcen",1), ((get_base1(cumyrsexp,n,"cumyrsexp",1) - meancumyrsexp) / sdcumyrsexp));
                current_statement_begin__ = 85;
                stan::math::assign(get_base1_lhs(cumwlm2lagcen,n,"cumwlm2lagcen",1), ((get_base1(cumwlm2lag,n,"cumwlm2lag",1) - meancumwlm2lag) / sdcumwlm2lag));
            }
            current_statement_begin__ = 88;
            stan::math::assign(maxX, max(cumwlm2lag));
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e,current_statement_begin__);
            // Next line prevents compiler griping about no return
throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        // validate transformed data

        // set parameter ranges
        num_params_r__ = 0U;
        param_ranges_i__.clear();
        ++num_params_r__;
        ++num_params_r__;
    }

    ~cpum_err_20151105_model() { }


    void transform_inits(const stan::io::var_context& context__,
                         std::vector<int>& params_i__,
                         std::vector<double>& params_r__,
                         std::ostream* pstream__) const {
        stan::io::writer<double> writer__(params_r__,params_i__);
        size_t pos__;
        (void) pos__; // dummy call to supress warning
        std::vector<double> vals_r__;
        std::vector<int> vals_i__;

        if (!(context__.contains_r("a0")))
            throw std::runtime_error("variable a0 missing");
        vals_r__ = context__.vals_r("a0");
        pos__ = 0U;
        context__.validate_dims("initialization", "a0", "double", context__.to_vec());
        double a0(0);
        a0 = vals_r__[pos__++];
        try {
            writer__.scalar_unconstrain(a0);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable a0: ") + e.what());
        }

        if (!(context__.contains_r("err")))
            throw std::runtime_error("variable err missing");
        vals_r__ = context__.vals_r("err");
        pos__ = 0U;
        context__.validate_dims("initialization", "err", "double", context__.to_vec());
        double err(0);
        err = vals_r__[pos__++];
        try {
            writer__.scalar_lb_unconstrain((-(1) / maxX),err);
        } catch (const std::exception& e) { 
            throw std::runtime_error(std::string("Error transforming variable err: ") + e.what());
        }

        params_r__ = writer__.data_r();
        params_i__ = writer__.data_i();
    }

    void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                         std::ostream* pstream__) const {
      std::vector<double> params_r_vec;
      std::vector<int> params_i_vec;
      transform_inits(context, params_i_vec, params_r_vec, pstream__);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r(i) = params_r_vec[i];
    }


    template <bool propto__, bool jacobian__, typename T__>
    T__ log_prob(vector<T__>& params_r__,
                 vector<int>& params_i__,
                 std::ostream* pstream__ = 0) const {

        T__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning

        T__ lp__(0.0);
        stan::math::accumulator<T__> lp_accum__;

        // model parameters
        stan::io::reader<T__> in__(params_r__,params_i__);

        T__ a0;
        (void) a0;   // dummy to suppress unused var warning
        if (jacobian__)
            a0 = in__.scalar_constrain(lp__);
        else
            a0 = in__.scalar_constrain();

        T__ err;
        (void) err;   // dummy to suppress unused var warning
        if (jacobian__)
            err = in__.scalar_lb_constrain((-(1) / maxX),lp__);
        else
            err = in__.scalar_lb_constrain((-(1) / maxX));


        // transformed parameters

        // initialize transformed variables to avoid seg fault on val access

        try {
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e,current_statement_begin__);
            // Next line prevents compiler griping about no return
throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        // validate transformed parameters

        const char* function__ = "validate transformed params";
        (void) function__; // dummy to suppress unused var warning

        // model body
        try {
            {
                Eigen::Matrix<T__,1,Eigen::Dynamic>  lambda(obscomplete);
                (void) lambda;  // dummy to suppress unused var warning
                stan::math::fill(lambda,DUMMY_VAR__);
                stan::math::initialize(lambda, DUMMY_VAR__);
                current_statement_begin__ = 113;
                for (int n = 1; n <= obscomplete; ++n) {
                    current_statement_begin__ = 114;
                    stan::math::assign(get_base1_lhs(lambda,n,"lambda",1), ((get_base1(py,n,"py",1) * exp(a0)) * (1 + ((err * get_base1(cumwlm2lag,n,"cumwlm2lag",1)) / 100))));
                }
                current_statement_begin__ = 118;
                lp_accum__.add(poisson_log<propto__>(d_lc, lambda));
            }
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e,current_statement_begin__);
            // Next line prevents compiler griping about no return
throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        lp_accum__.add(lp__);
        return lp_accum__.sum();

    } // log_prob()

    template <bool propto, bool jacobian, typename T_>
    T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
               std::ostream* pstream = 0) const {
      std::vector<T_> vec_params_r;
      vec_params_r.reserve(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        vec_params_r.push_back(params_r(i));
      std::vector<int> vec_params_i;
      return log_prob<propto,jacobian,T_>(vec_params_r, vec_params_i, pstream);
    }


    void get_param_names(std::vector<std::string>& names__) const {
        names__.resize(0);
        names__.push_back("a0");
        names__.push_back("err");
    }


    void get_dims(std::vector<std::vector<size_t> >& dimss__) const {
        dimss__.resize(0);
        std::vector<size_t> dims__;
        dims__.resize(0);
        dimss__.push_back(dims__);
        dims__.resize(0);
        dimss__.push_back(dims__);
    }

    template <typename RNG>
    void write_array(RNG& base_rng__,
                     std::vector<double>& params_r__,
                     std::vector<int>& params_i__,
                     std::vector<double>& vars__,
                     bool include_tparams__ = true,
                     bool include_gqs__ = true,
                     std::ostream* pstream__ = 0) const {
        vars__.resize(0);
        stan::io::reader<double> in__(params_r__,params_i__);
        static const char* function__ = "cpum_err_20151105_model_namespace::write_array";
        (void) function__; // dummy call to supress warning
        // read-transform, write parameters
        double a0 = in__.scalar_constrain();
        double err = in__.scalar_lb_constrain((-(1) / maxX));
        vars__.push_back(a0);
        vars__.push_back(err);

        if (!include_tparams__) return;
        // declare and define transformed parameters
        double lp__ = 0.0;
        (void) lp__; // dummy call to supress warning
        stan::math::accumulator<double> lp_accum__;


        try {
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e,current_statement_begin__);
            // Next line prevents compiler griping about no return
throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        // validate transformed parameters

        // write transformed parameters

        if (!include_gqs__) return;
        // declare and define generated quantities

        double DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
        (void) DUMMY_VAR__;  // suppress unused var warning


        // initialize transformed variables to avoid seg fault on val access

        try {
        } catch (const std::exception& e) {
            stan::lang::rethrow_located(e,current_statement_begin__);
            // Next line prevents compiler griping about no return
throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***");
        }

        // validate generated quantities

        // write generated quantities
    }

    template <typename RNG>
    void write_array(RNG& base_rng,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                     Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                     bool include_tparams = true,
                     bool include_gqs = true,
                     std::ostream* pstream = 0) const {
      std::vector<double> params_r_vec(params_r.size());
      for (int i = 0; i < params_r.size(); ++i)
        params_r_vec[i] = params_r(i);
      std::vector<double> vars_vec;
      std::vector<int> params_i_vec;
      write_array(base_rng,params_r_vec,params_i_vec,vars_vec,include_tparams,include_gqs,pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i)
        vars(i) = vars_vec[i];
    }

    static std::string model_name() {
        return "cpum_err_20151105_model";
    }


    void constrained_param_names(std::vector<std::string>& param_names__,
                                 bool include_tparams__ = true,
                                 bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "a0";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "err";
        param_names__.push_back(param_name_stream__.str());

        if (!include_gqs__ && !include_tparams__) return;

        if (!include_gqs__) return;
    }


    void unconstrained_param_names(std::vector<std::string>& param_names__,
                                   bool include_tparams__ = true,
                                   bool include_gqs__ = true) const {
        std::stringstream param_name_stream__;
        param_name_stream__.str(std::string());
        param_name_stream__ << "a0";
        param_names__.push_back(param_name_stream__.str());
        param_name_stream__.str(std::string());
        param_name_stream__ << "err";
        param_names__.push_back(param_name_stream__.str());

        if (!include_gqs__ && !include_tparams__) return;

        if (!include_gqs__) return;
    }

}; // model

} // namespace

typedef cpum_err_20151105_model_namespace::cpum_err_20151105_model stan_model;
