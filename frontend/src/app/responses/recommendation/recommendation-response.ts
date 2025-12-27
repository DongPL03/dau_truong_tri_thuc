import { CourseRecommendationItemResponse } from './course-recommendation-item-response';
import { BoCauHoiRecommendationItemResponse } from './bo-cau-hoi-recommendation-item-response';

export interface RecommendationResponse {
  courses: CourseRecommendationItemResponse[];
  bo_cau_hoi: BoCauHoiRecommendationItemResponse[];
}


