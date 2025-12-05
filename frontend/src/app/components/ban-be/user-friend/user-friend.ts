import {Component, OnInit} from '@angular/core';
import {Base} from '../../base/base';
import {FriendRequestItemResponse} from '../../../responses/banbe/friend_request_item_response';
import {FriendSummaryResponse} from '../../../responses/banbe/friend_summary_response';
import Swal from 'sweetalert2';
import {FormsModule} from '@angular/forms';
import {DatePipe} from '@angular/common';

@Component({
  selector: 'app-user-friend',
  imports: [
    FormsModule,
    DatePipe
  ],
  templateUrl: './user-friend.html',
  styleUrl: './user-friend.scss',
  standalone: true
})
export class UserFriend extends Base implements OnInit {
  // Tab hiện tại: 'friends' | 'incoming' | 'outgoing'
  active_tab: 'friends' | 'incoming' | 'outgoing' = 'friends';

  // Loading flags
  loading_friends = false;
  loading_incoming = false;
  loading_outgoing = false;

  // Dữ liệu
  friends: FriendSummaryResponse[] = [];
  incoming_requests: FriendRequestItemResponse[] = [];
  outgoing_requests: FriendRequestItemResponse[] = [];

  // Gửi lời mời nhanh theo user_id (tạm thời, sau này có search theo username/profile thì nâng cấp)
  new_friend_id: number | null = null;
  sending_request = false;

  ngOnInit(): void {
    // Mặc định load tab bạn bè
    this.loadFriends();
  }

  // ----- TAB -----
  setTab(tab: 'friends' | 'incoming' | 'outgoing'): void {
    this.active_tab = tab;

    if (tab === 'friends' && this.friends.length === 0) {
      this.loadFriends();
    } else if (tab === 'incoming' && this.incoming_requests.length === 0) {
      this.loadIncoming();
    } else if (tab === 'outgoing' && this.outgoing_requests.length === 0) {
      this.loadOutgoing();
    }
  }

  // ----- LOAD DATA -----
  loadFriends(): void {
    this.loading_friends = true;
    this.friendService.getFriends().subscribe({
      next: (res) => {
        this.friends = res.data || [];
        this.loading_friends = false;
      },
      error: (err) => {
        console.error('loadFriends error', err);
        this.loading_friends = false;
        Swal.fire('Lỗi', 'Không thể tải danh sách bạn bè', 'error');
      }
    });
  }

  loadIncoming(): void {
    this.loading_incoming = true;
    this.friendService.getIncomingRequests().subscribe({
      next: (res) => {
        this.incoming_requests = res.data || [];
        this.loading_incoming = false;
      },
      error: (err) => {
        console.error('loadIncoming error', err);
        this.loading_incoming = false;
        Swal.fire('Lỗi', 'Không thể tải danh sách lời mời đến', 'error');
      }
    });
  }

  loadOutgoing(): void {
    this.loading_outgoing = true;
    this.friendService.getOutgoingRequests().subscribe({
      next: (res) => {
        this.outgoing_requests = res.data || [];
        this.loading_outgoing = false;
      },
      error: (err) => {
        console.error('loadOutgoing error', err);
        this.loading_outgoing = false;
        Swal.fire('Lỗi', 'Không thể tải danh sách lời mời đã gửi', 'error');
      }
    });
  }

  // ----- ACTIONS -----

  /** Gửi lời mời kết bạn nhanh bằng user_id */
  sendFriendRequest(): void {
    if (!this.new_friend_id || this.new_friend_id <= 0) {
      Swal.fire('Thông báo', 'Vui lòng nhập user ID hợp lệ', 'info');
      return;
    }

    this.sending_request = true;
    this.friendService.sendRequest({target_user_id: this.new_friend_id}).subscribe({
      next: (res) => {
        this.sending_request = false;
        Swal.fire('Thành công', 'Đã gửi lời mời kết bạn', 'success');
        this.new_friend_id = null;
        // reload danh sách đã gửi (nếu đang ở tab đó)
        if (this.active_tab === 'outgoing') {
          this.loadOutgoing();
        }
      },
      error: (err) => {
        this.sending_request = false;
        console.error('sendFriendRequest error', err);
        const msg = err?.error?.message || 'Gửi lời mời kết bạn thất bại';
        Swal.fire('Lỗi', msg, 'error');
      }
    });
  }

  /** Chấp nhận lời mời đến */
  acceptRequest(req: FriendRequestItemResponse): void {
    this.friendService.acceptRequest(req.request_id).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Đã chấp nhận lời mời kết bạn', 'success');
        this.loadIncoming();
        this.loadFriends();
      },
      error: (err) => {
        console.error('acceptRequest error', err);
        const msg = err?.error?.message || 'Không thể chấp nhận lời mời';
        Swal.fire('Lỗi', msg, 'error');
      }
    });
  }

  /** Từ chối lời mời đến */
  declineRequest(req: FriendRequestItemResponse): void {
    this.friendService.declineRequest(req.request_id).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Đã từ chối lời mời kết bạn', 'success');
        this.loadIncoming();
      },
      error: (err) => {
        console.error('declineRequest error', err);
        const msg = err?.error?.message || 'Không thể từ chối lời mời';
        Swal.fire('Lỗi', msg, 'error');
      }
    });
  }

  /** Huỷ lời mời đã gửi */
  cancelRequest(req: FriendRequestItemResponse): void {
    this.friendService.cancelRequest(req.request_id).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Đã huỷ lời mời kết bạn', 'success');
        this.loadOutgoing();
      },
      error: (err) => {
        console.error('cancelRequest error', err);
        const msg = err?.error?.message || 'Không thể huỷ lời mời';
        Swal.fire('Lỗi', msg, 'error');
      }
    });
  }

  /** Huỷ kết bạn */
  unfriend(friend: FriendSummaryResponse): void {
    Swal.fire({
      title: 'Xác nhận',
      text: `Bạn có chắc muốn huỷ kết bạn với ${friend.ho_ten}?`,
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Huỷ kết bạn',
      cancelButtonText: 'Đóng'
    }).then(result => {
      if (!result.isConfirmed) return;

      this.friendService.unfriend(friend.user_id).subscribe({
        next: () => {
          Swal.fire('Thành công', 'Đã huỷ kết bạn', 'success');
          this.loadFriends();
        },
        error: (err) => {
          console.error('unfriend error', err);
          const msg = err?.error?.message || 'Không thể huỷ kết bạn';
          Swal.fire('Lỗi', msg, 'error');
        }
      });
    });
  }

}
