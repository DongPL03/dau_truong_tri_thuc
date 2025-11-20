export interface SyncStateResponse {
  tran_dau_id: number;
  current_question_index: number;
  current_question_id: number;
  seconds_per_question: number;
  current_question_start: string;
  noi_dung: string;
  loai_noi_dung: string;
  duong_dan_tep?: string;
  a: string;
  b: string;
  c: string;
  d: string;
  my_total_points: number;

  _version?: number;
}
