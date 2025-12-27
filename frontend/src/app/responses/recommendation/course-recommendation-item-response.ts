export interface CourseRecommendationItemResponse {
  khoa_hoc: {
    id: number;
    tieu_de: string;
    chu_de?: string | null;
    mo_ta?: string | null;
  };
  ly_do: string;
}


