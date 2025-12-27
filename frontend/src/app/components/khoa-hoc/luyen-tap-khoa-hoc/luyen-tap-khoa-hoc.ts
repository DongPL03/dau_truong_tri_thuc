import { CommonModule } from '@angular/common';
import { Component, computed, OnDestroy, OnInit, signal } from '@angular/core';
import { environment } from 'src/app/environments/environment';
import Swal from 'sweetalert2';
import {
  CauTraLoiPracticeDTO,
  TraLoiCauHoiPracticeDTO,
} from '../../../dtos/luyen-tap/tra-loi-cau-hoi-dto';
import {
  BatDauLuyenTapResponse,
  CauHoiPracticeItem,
} from '../../../responses/luyentap/bat_dau_luyen_tap-response';
import { SubmitLuyenTapResponse } from '../../../responses/luyentap/submit_luyen_tap-response';
import { ResponseObject } from '../../../responses/response-object';
import { Base } from '../../base/base';

interface LocalAnswerState {
  lua_chon?: 'A' | 'B' | 'C' | 'D' | null;
  start_time_ms: number;
  elapsed_ms?: number;
}

@Component({
  selector: 'app-luyen-tap-khoa-hoc',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './luyen-tap-khoa-hoc.html',
  styleUrl: './luyen-tap-khoa-hoc.scss',
})
export class LuyenTapKhoaHocComponent extends Base implements OnInit, OnDestroy {
  khoaHocId: number | null = null;
  boCauHoiId: number | null = null;

  loading = signal<boolean>(false);
  playing = signal<boolean>(false);
  submitting = signal<boolean>(false);
  finished = signal<boolean>(false);

  practice_data = signal<BatDauLuyenTapResponse | null>(null);
  current_index = signal<number>(0);

  private answers_map = new Map<number, LocalAnswerState>();
  private answers_version = signal<number>(0);

  result = signal<SubmitLuyenTapResponse | null>(null);

  seconds_per_question = 15;
  remaining_seconds = signal<number>(0);
  private questionTimer?: ReturnType<typeof setInterval>;

  protected readonly environment = environment;

  practiceFromMemo = false; // Flag để biết là practice từ memo

  ngOnInit(): void {
    this.route.paramMap.subscribe((params) => {
      const khoaHocIdParam = params.get('khoaHocId');
      const boCauHoiIdParam = params.get('boCauHoiId');

      this.khoaHocId = khoaHocIdParam ? Number(khoaHocIdParam) : null;
      this.boCauHoiId = boCauHoiIdParam ? Number(boCauHoiIdParam) : null;

      // Check xem có 'memo' trong URL path không (không phải param)
      const urlSegments = this.route.snapshot.url.map((segment) => segment.path);
      this.practiceFromMemo = urlSegments.includes('memo');

      console.log(
        'DEBUG LuyenTapKhoaHocComponent: urlSegments =',
        urlSegments,
        ', practiceFromMemo =',
        this.practiceFromMemo
      );

      if (this.khoaHocId && this.boCauHoiId) {
        if (this.practiceFromMemo) {
          this.startPracticeFromMemo();
        } else {
          this.startPractice();
        }
      } else {
        Swal.fire('Lỗi', 'Thiếu thông tin khóa học hoặc bộ câu hỏi', 'error').then(() => {
          this.router.navigate(['/khoa-hoc']);
        });
      }
    });
  }

  ngOnDestroy() {
    this.clearTimer();
  }

  startPractice() {
    if (!this.boCauHoiId) {
      Swal.fire('Lỗi', 'Không tìm thấy bộ câu hỏi', 'error');
      return;
    }

    this.loading.set(true);
    const dto = {
      bo_cau_hoi_id: this.boCauHoiId,
      so_luong: 0,
    };

    this.luyenTapService.startPractice(dto).subscribe({
      next: (res: ResponseObject<BatDauLuyenTapResponse>) => {
        this.loading.set(false);
        const data = res.data!;
        if (!data || !data.cau_hoi_list || data.cau_hoi_list.length === 0) {
          Swal.fire('Không có câu hỏi', 'Bộ câu hỏi này chưa có câu hỏi', 'info').then(() => {
            this.router.navigate(['/khoa-hoc', this.khoaHocId]);
          });
          return;
        }

        this.setupPracticeSession(data);
      },
      error: (err) => {
        this.loading.set(false);
        Swal.fire('Lỗi', err?.error?.message || 'Không thể bắt đầu luyện tập', 'error').then(() => {
          this.router.navigate(['/khoa-hoc', this.khoaHocId]);
        });
      },
    });
  }

  startPracticeFromMemo() {
    if (!this.boCauHoiId) {
      Swal.fire('Lỗi', 'Không tìm thấy bộ câu hỏi', 'error');
      return;
    }

    this.loading.set(true);
    this.luyenTapService.startPracticeFromMemos(this.boCauHoiId).subscribe({
      next: (res: ResponseObject<BatDauLuyenTapResponse>) => {
        this.loading.set(false);
        const data = res.data!;
        console.log('DEBUG startPracticeFromMemo: Response từ API:', data);
        console.log('DEBUG startPracticeFromMemo: Số lượng câu hỏi =', data?.cau_hoi_list?.length);
        if (!data || !data.cau_hoi_list || data.cau_hoi_list.length === 0) {
          Swal.fire(
            'Không có câu hỏi',
            'Bạn chưa có câu hỏi nào cần ôn lại trong bộ câu hỏi này',
            'info'
          ).then(() => {
            this.router.navigate(['/khoa-hoc', this.khoaHocId]);
          });
          return;
        }

        this.setupPracticeSession(data);
      },
      error: (err) => {
        this.loading.set(false);
        Swal.fire(
          'Lỗi',
          err?.error?.message || 'Không thể bắt đầu luyện tập từ thẻ ghi nhớ',
          'error'
        ).then(() => {
          this.router.navigate(['/khoa-hoc', this.khoaHocId]);
        });
      },
    });
  }

  private setupPracticeSession(data: BatDauLuyenTapResponse) {
    this.practice_data.set(data);
    this.current_index.set(0);
    this.answers_map.clear();
    this.answers_version.update((v) => v + 1);
    this.result.set(null);
    this.playing.set(true);
    this.finished.set(false);

    this.startQuestion(0);

    const firstQ = data.cau_hoi_list[0];
    this.answers_map.set(firstQ.id, {
      lua_chon: null,
      start_time_ms: Date.now(),
    });
  }

  currentQuestion = computed<CauHoiPracticeItem | null>(() => {
    const data = this.practice_data();
    const idx = this.current_index();
    if (!data || !data.cau_hoi_list || idx < 0 || idx >= data.cau_hoi_list.length) {
      return null;
    }
    return data.cau_hoi_list[idx];
  });

  currentAnswer = computed<'A' | 'B' | 'C' | 'D' | ''>(() => {
    const q = this.currentQuestion();
    this.answers_version();
    if (!q) return '';
    const st = this.answers_map.get(q.id);
    return st?.lua_chon ?? '';
  });

  private startQuestion(index: number) {
    const data = this.practice_data();
    if (!data || index < 0 || index >= data.cau_hoi_list.length) return;

    this.current_index.set(index);

    const q = data.cau_hoi_list[index];
    const now = Date.now();

    const existed = this.answers_map.get(q.id);
    if (!existed) {
      this.answers_map.set(q.id, {
        lua_chon: null,
        start_time_ms: now,
      });
    } else {
      existed.start_time_ms = now;
      this.answers_map.set(q.id, existed);
    }

    this.startTimer();
  }

  private startTimer() {
    this.clearTimer();
    this.remaining_seconds.set(this.seconds_per_question);

    this.questionTimer = setInterval(() => {
      const r = this.remaining_seconds() - 1;
      this.remaining_seconds.set(r);

      if (r <= 0) {
        this.handleTimeout();
      }
    }, 1000);
  }

  private saveElapsedForCurrent() {
    const q = this.currentQuestion();
    if (!q) return;

    const st = this.answers_map.get(q.id);
    if (!st) return;

    const used = (this.seconds_per_question - this.remaining_seconds()) * 1000;
    st.elapsed_ms = used > 0 ? used : 0;
    this.answers_map.set(q.id, st);
  }

  private clearTimer() {
    if (this.questionTimer) {
      clearInterval(this.questionTimer);
      this.questionTimer = undefined;
    }
  }

  submitCurrentQuestion() {
    const q = this.currentQuestion();
    if (!q) return;

    const answer = this.currentAnswer();
    if (!answer) {
      Swal.fire('Chưa chọn đáp án', 'Hãy chọn A/B/C/D trước khi nộp', 'info');
      return;
    }

    this.saveElapsedForCurrent();
    this.goNextOrFinish();
  }

  private handleTimeout() {
    this.saveElapsedForCurrent();
    this.goNextOrFinish();
  }

  private goNextOrFinish() {
    const data = this.practice_data();
    if (!data) return;

    this.clearTimer();

    const idx = this.current_index();
    const isLast = idx >= data.cau_hoi_list.length - 1;

    if (!isLast) {
      this.startQuestion(idx + 1);
    } else {
      this.onSubmitPractice();
    }
  }

  selectAnswer(opt: 'A' | 'B' | 'C' | 'D') {
    const q = this.currentQuestion();
    if (!q) return;

    const existed = this.answers_map.get(q.id);
    if (!existed) {
      this.answers_map.set(q.id, {
        lua_chon: opt,
        start_time_ms: Date.now(),
      });
    } else {
      existed.lua_chon = opt;
      this.answers_map.set(q.id, existed);
    }

    this.answers_version.update((v) => v + 1);
  }

  goPrev() {
    const data = this.practice_data();
    const idx = this.current_index();
    if (!data || idx <= 0) return;
    this.clearTimer();
    this.startQuestion(idx - 1);
  }

  goNext() {
    const data = this.practice_data();
    const idx = this.current_index();
    if (!data || idx >= data.cau_hoi_list.length - 1) return;
    this.clearTimer();
    this.startQuestion(idx + 1);
  }

  onSubmitPractice() {
    const data = this.practice_data();
    if (!data) return;

    const phien_id = data.phien_id;

    const cau_tra_loi_list: CauTraLoiPracticeDTO[] = data.cau_hoi_list.map((q) => {
      const st = this.answers_map.get(q.id);
      return {
        cau_hoi_id: q.id,
        lua_chon: st?.lua_chon ?? null,
        thoi_gian_ms: st?.elapsed_ms ?? null,
      };
    });

    const dto: TraLoiCauHoiPracticeDTO = {
      phien_id,
      cau_tra_loi_list,
    };

    this.submitting.set(true);
    this.luyenTapService.submitPractice(dto).subscribe({
      next: (res: ResponseObject<SubmitLuyenTapResponse>) => {
        this.submitting.set(false);
        this.result.set(res.data!);
        this.finished.set(true);
        this.playing.set(false);

        // Hiển thị thông báo và redirect về chi tiết khóa học sau 2 giây
        Swal.fire({
          title: 'Hoàn thành!',
          text: `Điểm: ${res.data!.diem_so}/${res.data!.tong_so_cau} - Độ chính xác: ${
            res.data!.do_chinh_xac
          }%`,
          icon: 'success',
          timer: 2000,
          showConfirmButton: false,
        }).then(() => {
          // Đợi một chút để đảm bảo backend đã commit transaction và unlock bộ tiếp theo
          setTimeout(() => {
            // Navigate về chi tiết khóa học và reload để cập nhật progress
            if (this.khoaHocId) {
              this.router.navigate(['/khoa-hoc', this.khoaHocId]).then(() => {
                // Reload để đảm bảo data mới nhất được load
                window.location.reload();
              });
            } else {
              this.router.navigate(['/khoa-hoc']);
            }
          }, 1000); // Delay 1 giây để đảm bảo backend đã commit
        });
      },
      error: (err) => {
        this.submitting.set(false);
        Swal.fire('Lỗi', err?.error?.message || 'Không thể nộp bài luyện tập', 'error');
      },
    });
  }

  backToCourse() {
    this.router.navigate(['/khoa-hoc', this.khoaHocId]);
  }
}
