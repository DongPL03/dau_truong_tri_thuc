export interface NotificationResponse {
  thong_bao_id: number;
  nguoi_gui_id: number;
  nguoi_gui_ten: string;
  nguoi_gui_avatar_url: string | null;
  loai:
    | 'FRIEND_REQUEST'
    | 'BATTLE_INVITE'
    | 'SYSTEM'
    | 'QUIZ_APPROVED'
    | 'QUIZ_UNLOCKED'
    | string;
  noi_dung: string;
  metadata: string | null;
  da_doc: boolean;
  tao_luc: string;
}
