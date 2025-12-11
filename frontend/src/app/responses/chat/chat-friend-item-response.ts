export interface ChatFriendItemResponse {
  // id của người bạn (đối tác chat)
  partner_id: number;

  // tên hiển thị
  partner_name: string;

  // avatar (relative url hoặc absolute url)
  partner_avatar_url?: string | null;

  // trạng thái online/offline (chuỗi, ví dụ: 'ONLINE' | 'OFFLINE')
  partner_status?: string | null;

  // tin nhắn mới nhất giữa 2 người (nếu có)
  last_message?: string | null;

  // thời gian gửi tin nhắn mới nhất (ISO string / Date)
  last_time?: string | null | Date;

  // số tin nhắn chưa đọc (hiện tại có thể chưa dùng, mặc định coi như 0)
  unread_count?: number;
}

