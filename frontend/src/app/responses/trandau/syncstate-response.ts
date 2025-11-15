// export interface SyncStateResponse {
//   tran_dau_id: number;
//   trang_thai: 'PENDING' | 'ONGOING' | 'FINISHED';
//   current_question_index: number; // -1 if none
//   current_question_start?: string | null; // ISO
//   seconds_per_question: number;
//   current_question_id?: number | null;
//   noi_dung?: string | null;
//   loai_noi_dung?: 'VAN_BAN' | 'HINH_ANH' | 'AM_THANH' | 'VIDEO';
//   duong_dan_tep?: string;
//   a?: string | null; b?: string | null; c?: string | null; d?: string | null;
//   my_total_points: number;
// }
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
