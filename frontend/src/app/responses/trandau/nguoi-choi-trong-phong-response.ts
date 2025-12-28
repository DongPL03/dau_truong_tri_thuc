/**
 * Response thông tin người chơi trong phòng (trước khi trận đấu bắt đầu)
 */
export interface NguoiChoiTrongPhongResponse {
  user_id: number;
  ho_ten: string;
  avatar_url: string | null;
  la_chu_phong: boolean;
  da_san_sang: boolean;
  tham_gia_luc: string | null;
}
