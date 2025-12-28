/**
 * Response thông tin người dùng bị chặn
 */
export interface BlockedUserResponse {
  block_id: number;
  user_id: number;
  ho_ten: string;
  avatar_url: string | null;
  ly_do: string | null;
  chan_luc: string;
}
